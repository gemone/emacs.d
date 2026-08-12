;;; init-treesit.el --- treesit-auto and the tree-sitter CLI installer -*- lexical-binding: t; -*-

;;; Commentary:
;; tree-sitter integration: `treesit-auto' maps languages to their
;; tree-sitter major modes and prompts for missing grammars.  Grammar
;; installs are rerouted to the `tree-sitter' CLI (`tree-sitter generate
;; --abi N' + `tree-sitter build'), which produces grammars matching this
;; Emacs' ABI, and run in a detached batch subprocess so startup and
;; editing are never blocked.

;;; Code:
;; ts
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  ;; Prompt to install missing grammars when visiting a file (uses the CLI
  ;; install path below); set to t for silent auto-install.
  (setq treesit-auto-install 'prompt)
  ;; zig is handled by zig-mode below; keep treesit-auto from installing the
  ;; zig grammar and remapping .zig/.zon to the built-in zig-ts-mode.
  (setq treesit-auto-langs (delq 'zig treesit-auto-langs))

  ;; Install tree-sitter grammars via the `tree-sitter' CLI
  ;; (tree-sitter-cli) instead of Emacs's built-in cc compilation:
  ;; `tree-sitter generate --abi N' regenerates parser.c at the ABI
  ;; version of this Emacs, and `tree-sitter build' handles scanners and
  ;; linking.  Output goes straight into the cache directory (already on
  ;; `treesit-extra-load-path').  Falls back to the built-in installer if
  ;; `tree-sitter' is not on PATH.
  (when (fboundp 'treesit-install-language-grammar)
    ;; `tree-sitter' is resolved through the system PATH (cargo installs it
    ;; to ~/.cargo/bin); make sure that directory is on the OS PATH and
    ;; restart Emacs (or the daemon) so the new environment is inherited.

    (defun my/treesit-cli-run (&rest args)
      "Run ARGS as a subprocess; signal `treesit-error' on failure."
      (with-temp-buffer
        (unless (eq 0 (apply #'call-process (car args) nil t nil (cdr args)))
          (signal 'treesit-error
                  (list (string-join (cons (car args) (cdr args)) " ")
                        (buffer-string))))))

    (defun my/treesit-cli-grammar-dir (repo &optional source-dir)
      "Locate the directory holding the grammar to build in REPO."
      (let* ((default-directory repo)
             (candidates (if source-dir (list source-dir ".") '(".")))
             (dir (seq-find
                   (lambda (d)
                     (or (file-exists-p (expand-file-name "grammar.js" d))
                         (file-exists-p (expand-file-name "grammar.json" d))))
                   candidates)))
        (expand-file-name (or dir (or source-dir "src")))))

    (defun my/treesit-cli-install-language-grammar (lang &optional out-dir)
      "Install grammar LANG using the `tree-sitter' CLI.
Clones the recipe repo, regenerates parser.c at the ABI version of
this Emacs, builds the shared library with `tree-sitter build' and
copies it to OUT-DIR (default the cache grammar dir)."
      (let* ((recipe (assoc lang treesit-language-source-alist))
             (url (nth 1 recipe))
             (revision (nth 2 recipe))
             (source-dir (nth 3 recipe))
             (workdir (make-temp-file "treesit-workdir" t))
             (repo (expand-file-name "repo" workdir))
             (out-dir (expand-file-name
                       (or out-dir
                           (expand-file-name "tree-sitter/" my/cache-dir))))
             (abi (or (and (fboundp 'treesit-library-abi-version)
                           (treesit-library-abi-version))
                      14))
             (lib-name (format "libtree-sitter-%s%s"
                               lang (or (car dynamic-library-suffixes) ".so"))))
        (unwind-protect
            (progn
              (unless url
                (signal 'treesit-error
                        (list "No recipe for" lang
                              "in `treesit-language-source-alist'")))
              (message "tree-sitter: cloning %s" url)
              (if revision
                  (my/treesit-cli-run "git" "clone" "--depth" "1" "--quiet"
                                      "-b" revision url repo)
                (my/treesit-cli-run "git" "clone" "--depth" "1" "--quiet"
                                    url repo))
              (let* ((grammar-dir (my/treesit-cli-grammar-dir repo source-dir))
                     (default-directory grammar-dir))
                (when (or (file-exists-p "grammar.js")
                          (file-exists-p "grammar.json"))
                  (message "tree-sitter: generating parser for %s (ABI %d)"
                           lang abi)
                  (my/treesit-cli-run "tree-sitter" "generate"
                                      "--abi" (number-to-string abi)))
                (unless (file-exists-p out-dir)
                  (make-directory out-dir t))
                (message "tree-sitter: building %s" lib-name)
                (my/treesit-cli-run "tree-sitter" "build"
                                    "-o" (expand-file-name lib-name out-dir))))
          (ignore-errors (delete-directory workdir t)))
        ;; Mirror the built-in behavior: verify the grammar loads after install.
        (pcase-let ((`(,available . ,err)
                     (treesit-language-available-p lang t)))
          (if (not available)
              (progn
                (display-warning
                 'treesit
                 (format "tree-sitter CLI install failed for %s: %s"
                         lang (mapconcat (lambda (x) (format "%s" x)) err " ")))
                t)
            (message "tree-sitter: %s installed to %s" lang out-dir)
            nil))))

    (defun my/treesit-install-async (lang)
      "Install grammar LANG in a detached Emacs batch process.
The subprocess reuses the CLI installer above, so the frontend is
never blocked while the grammar is downloaded and built."
      (let* ((emacs (expand-file-name invocation-name invocation-directory))
             (buf (get-buffer-create (format "*treesit-install-%s*" lang)))
             (target (current-buffer))
             (body
              (format
               (concat
                "(progn "
                "(require 'treesit) "
                "(defvar my/cache-dir %S) "
                "(setq treesit-language-source-alist %S) "
                "(setq treesit-extra-load-path %S) "
                "(setf (symbol-function 'my/treesit-cli-run) %S) "
                "(setf (symbol-function 'my/treesit-cli-grammar-dir) %S) "
                "(setf (symbol-function 'my/treesit-cli-install-language-grammar) %S) "
                "(let ((failed (if (executable-find \"tree-sitter\") "
                "(my/treesit-cli-install-language-grammar %S nil) "
                "(treesit-install-language-grammar %S nil)))) "
                "(kill-emacs (if (or failed (not (treesit-language-available-p %S))) 1 0))))")
               my/cache-dir
               treesit-language-source-alist
               treesit-extra-load-path
               (symbol-function 'my/treesit-cli-run)
               (symbol-function 'my/treesit-cli-grammar-dir)
               (symbol-function 'my/treesit-cli-install-language-grammar)
               lang lang lang)))
        (with-current-buffer buf (erase-buffer))
        (message "tree-sitter: installing %s grammar in background" lang)
        (make-process
         :name (format "treesit-install-%s" lang)
         :noquery t
         :buffer buf
         :command (list emacs "-Q" "--batch" "--eval" body)
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (if (zerop (process-exit-status proc))
                 (progn
                   (message "tree-sitter: %s grammar installed" lang)
                   ;; Re-enable the ts-mode in the buffer that triggered the
                   ;; install, so no manual reopen is needed.
                   (when (and (buffer-live-p target)
                              (fboundp 'treesit-auto--get-mode-recipe)
                              (fboundp 'treesit-auto--ready-p))
                     (with-current-buffer target
                       (when-let* ((recipe (treesit-auto--get-mode-recipe))
                                   (ts-mode (treesit-auto-recipe-ts-mode recipe))
                                   (grammar (treesit-auto-recipe-lang recipe))
                                   ((eq grammar lang))
                                   ((treesit-auto--ready-p ts-mode)))
                         (funcall ts-mode)
                         (message "tree-sitter: enabled %s" ts-mode)))))
               (message
                "tree-sitter: background install of %s failed; see *treesit-install-%s*"
                lang lang)))))))

    (defun my/treesit-install-via-cli (orig-fun lang &optional out-dir)
      "Install LANG in the background; fall back to ORIG-FUN when needed.
Background install is skipped for explicit interactive calls
(\`M-x treesit-install-language-grammar') or with a prefix arg."
      (if (or (treesit-language-available-p lang)
              (eq out-dir 'interactive)
              current-prefix-arg)
          (funcall orig-fun lang out-dir)
        (my/treesit-install-async lang)
        nil))

    (advice-add 'treesit-install-language-grammar
                :around #'my/treesit-install-via-cli)))

(provide 'init-treesit)

;;; init-treesit.el ends here
