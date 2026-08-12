;;; init.el --- user init -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal Emacs configuration.

;; The configuration is split into modular files under `lisp/', loaded
;; here in dependency order.  Each module is an Emacs feature
;; (`lisp/init-*.el' -> `(provide 'init-*)') with a single concern:

;;   lisp/init-package.el         Elpaca bootstrap + use-package bridge
;;   lisp/init-core.el            GC, performance, dirs, base UI, backups
;;   lisp/init-prog-modes.el      opt-in prog-mode framework + custom.el
;;   lisp/init-appearance.el      theme + fonts
;;   lisp/init-editing.el         meow, smartparens, paren/indent visuals
;;   lisp/init-completion.el      vertico/orderless/consult/corfu/cape
;;   lisp/init-navigation.el      ibuffer + isearch
;;   lisp/init-dired.el           dired (listing, subtree, narrow, wdired)
;;   lisp/init-coding.el          prog-mode line numbers, magit
;;   lisp/init-project.el         projectile + treemacs
;;   lisp/init-treesit.el         treesit-auto + tree-sitter CLI installer
;;   lisp/init-eglot.el           LSP client (eglot), eldoc-box, Python LSP
;;   lisp/init-lang-elisp.el      Emacs Lisp flymake
;;   lisp/init-lang-zig.el        Zig (zig-mode)
;;   lisp/init-lang-lisp.el       Common Lisp (SLIME)
;;   lisp/init-lang-java.el       Java (jdtls + eglot-java + dape + java-server)
;;   lisp/init-lang-web.el        TypeScript / Angular / Vue / web-mode
;;   lisp/init-lang-markdown.el   markdown-mode + edit-indirect
;;   lisp/init-extras.el          agent-shell, ghostel
;;   modules/                     your own development plugins (auto-loaded)

;; Configuration: everything `M-x customize' writes, plus the opt-in
;; variables (`my/install-prog-modes', `my/frontend-ts-contacts',
;; `my/frontend-web-contacts', `my/web-mode-auto-mode', ...), lives in
;; `custom.el' (gitignored; `custom-example.el' is the template).
;; Private development plugins go into `modules/'.
;; Reloading after editing a module or `custom.el':
;;   M-x my/reload-config, or  emacsclient --eval "(my/reload-config)"
;; (`init-package' is skipped on reload; restart Emacs to apply its changes.)

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Ordered configuration modules: the single source of truth for startup
;; order and for `my/reload-config'.
(defconst my/init-modules
  '(init-package init-core init-prog-modes init-appearance init-editing
    init-completion init-navigation init-dired init-coding init-project
    init-treesit
    init-eglot init-lang-elisp init-lang-zig init-lang-lisp init-lang-java
    init-lang-web init-lang-markdown init-extras)
  "Ordered list of configuration modules loaded by `init.el'.
`init-package' (the Elpaca bootstrap) must come first; `my/reload-config'
excludes it deliberately.")

(dolist (mod my/init-modules)
  (require mod))

;;; Custom development plugins
;; Drop your own packages/modules into `modules/'.  Every `*.el' file there
;; is added to `load-path' and loaded (as a feature, sorted by file name)
;; after all init-*.el modules are up, so plugins can `(require)' the init
;; modules they build on.  Give each plugin file a `(provide 'foo)' so
;; re-loading never double-loads it.
(defun my/modules-load (&optional force)
  "Load every `*.el' file in `modules/' in sorted order.
When FORCE is non-nil, `load' each file directly so a reload actually
re-evaluates it; otherwise `require' the derived feature (skips files
already loaded)."
  (let ((mod-dir (expand-file-name "modules/" user-emacs-directory)))
    (when (file-directory-p mod-dir)
      (add-to-list 'load-path mod-dir)
      (dolist (file (sort (directory-files mod-dir t "\\.el\\'" 'nosort)
                          #'string<))
        (when (file-regular-p file)
          (if force
              (load file 'noerror 'no-message)
            (require (intern (file-name-base file)) mod-dir 'noerror)))))))

(my/modules-load)

;;; Configuration reload
(defun my/reload-config ()
  "Re-evaluate every configuration module after `init-package'.
`init-package' (the Elpaca bootstrap) is excluded: re-running it would
re-enqueue Elpaca itself and can trigger a rebuild.  Modules are `load'ed
directly (bypassing `require''s feature cache) so their forms actually
re-run; `modules/' plugins are force-reloaded too.  The current
`my/install-prog-modes' (e.g. just edited in `custom.el') is picked up.
Use `M-x my/reload-config', or from the shell:
`emacsclient --eval \"(my/reload-config)\"'."
  (interactive)
  (dolist (mod (cdr my/init-modules))
    (condition-case-unless-debug err
        (progn
          (load (expand-file-name (format "%s.el" mod)
                                  (expand-file-name "lisp/" user-emacs-directory)))
          (message "Reloaded %s" mod))
       (error
        (message "Reload %s failed: %s" mod (error-message-string err)))))
  (my/modules-load t))

(provide 'init)

;;; init.el ends here
