;;; init.el --- user init -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal Emacs configuration.

;; The configuration is split into modular files under `lisp/', loaded
;; here in dependency order.  Each module is an Emacs feature
;; (`lisp/init-*.el' -> `(provide 'init-*)') with a single concern:

;;   lisp/init-package.el         Elpaca bootstrap + use-package bridge
;;   lisp/init-const.el           shared constants (runtime dirs), no side effects
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
;;   lisp/init-elisp.el           Emacs Lisp flymake (always loaded)
;;   lisp/init-lang-*.el          opt-in languages (gated by filename suffix)
;;   lisp/init-lang-zig.el        Zig (zig-mode)
;;   lisp/init-lang-common-lisp.el Common Lisp (SLIME)
;;   lisp/init-lang-java.el       Java (jdtls + eglot-java + dape + java-server)
;;   lisp/init-lang-web-basic.el  web-mode + TypeScript/HTML LSP
;;   lisp/init-lang-web-vue.el    Vue (vue-mode + Volar)
;;   lisp/init-lang-web-angular.el Angular (ngserver)
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
;; order and for `my/reload-config'.  Language modules are NOT listed here;
;; they are loaded separately via `my/load-language-modules', gated on
;; `my/install-prog-modes'.
(defconst my/init-modules
  '(init-package init-const init-core init-prog-modes init-appearance
    init-editing
    init-completion init-navigation init-dired init-coding init-project
    init-treesit
    init-eglot init-elisp init-extras)
  "Ordered list of core configuration modules loaded by `init.el'.
`init-package' (the Elpaca bootstrap) must come first; `my/reload-config'
excludes it deliberately.")

;;; Opt-in language modules
;; `init-lang-<lang>.el' modules are DISCOVERED automatically — pure
;; filename convention, no mapping: the gate symbol IS the filename suffix
;; after `init-lang-' (`init-lang-zig' -> gate `zig').  A module is loaded
;; only when its gate symbol is in `my/install-prog-modes' (see
;; `init-prog-modes'); disabled languages are never required, so their
;; packages are neither installed nor loaded.  The modules themselves are
;; gate-free — they only contain the language's configuration; whether
;; they run at all is decided here.
;; Adding a language = drop in `lisp/init-lang-<lang>.el' and add <lang>
;; via `M-x my/add-prog-modes'; nothing else to edit.
(defun my/load-language-modules (&optional force)
  "Load every `init-lang-*' module whose gate symbol is enabled.
The gate is the filename suffix after `init-lang-' (e.g. `init-lang-zig'
gates on `zig'); a module loads only when that symbol is in
`my/install-prog-modes'.  With FORCE non-nil, `load' each module directly
so reloading re-evaluates it (used by `my/reload-config'); otherwise
`require' the feature (skips modules already loaded)."
  (let ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
    (dolist (file (sort (directory-files lisp-dir t "^init-lang-.*\\.el\\'")
                        #'string<))
      (let* ((module (intern (file-name-base file)))
             (gate (intern (substring (symbol-name module)
                                      (length "init-lang-")))))
        (when (my/prog-mode-enabled-p gate)
          (if force
              (load file)
            (require module)))))))

(declare-function my/prog-mode-enabled-p "init-prog-modes")

(dolist (mod my/init-modules)
  (require mod))

(my/load-language-modules)

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
re-enqueue Elpaca itself and can trigger a rebuild.  Core modules are
`load'ed directly (bypassing `require''s feature cache) so their forms
actually re-run; the enabled `init-lang-*' modules are reloaded with the
same `my/install-prog-modes' gating as startup; `modules/' plugins are
force-reloaded too.  The current `my/install-prog-modes' (e.g. just
edited in `custom.el') is picked up.  Use `M-x my/reload-config', or from
the shell: `emacsclient --eval \"(my/reload-config)\"'."
  (interactive)
  (let ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
    (dolist (mod (cdr my/init-modules))
      (condition-case-unless-debug err
          (progn
            (load (expand-file-name (format "%s.el" mod) lisp-dir))
            (message "Reloaded %s" mod))
        (error
         (message "Reload %s failed: %s" mod (error-message-string err)))))
    ;; Reload the enabled language modules (same gating as startup).
    (condition-case-unless-debug err
        (my/load-language-modules t)
      (error
       (message "Reload language modules failed: %s"
                (error-message-string err)))))
  (my/modules-load t))

(provide 'init)

;;; init.el ends here
