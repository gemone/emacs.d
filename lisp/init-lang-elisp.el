;;; init-lang-elisp.el --- Emacs Lisp flymake -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp development support.  Emacs Lisp has no LSP server, so this
;; module only configures the built-in `flymake' backends: checkdoc
;; (documentation/style) is kept, byte-compilation is removed because the
;; subprocess `load-path' cannot see Elpaca packages and would produce
;; noise.

;;; Code:
;; These are defined in `checkdoc.el', which is loaded lazily by the
;; checkdoc flymake backend; declare them here to silence byte-compile.
(defvar checkdoc-proper-noun-list)
(defvar checkdoc-proper-noun-regexp)

;;; Elisp linting/static checks (Emacs Lisp has no LSP server)
(use-package elisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . my/elisp-flymake-setup)
  :config
  (defun my/elisp-flymake-setup ()
    "Enable flymake in elisp buffers and drop the noisy byte-compile backend.
Named function so `use-package :hook' dedups it across reloads.  The
byte-compile subprocess `load-path' only contains \"./\", so it cannot
see Elpaca packages and produces a lot of \"function not defined\" noise."
    (flymake-mode 1)
    (remove-hook 'flymake-diagnostic-functions
                 #'elisp-flymake-byte-compile t))
  ;; Emacs 30's `emacs-lisp-mode' registers two flymake backends by default:
  ;; `elisp-flymake-byte-compile' (compile errors; removed above) and
  ;; `elisp-flymake-checkdoc' (doc/style; kept).
  ;;
  ;; Silence checkdoc's proper-noun nag: its default list
  ;; ("emacs" "lisp" "dired") flags every lowercase occurrence in a
  ;; docstring ("Name dired should appear capitalized as Dired"), which is
  ;; pure noise.  Drop the list and rebuild the cached regexp so it can
  ;; never match.  Runs after checkdoc loads (it is lazily loaded by the
  ;; checkdoc flymake backend), so the defvars have already run.
  (with-eval-after-load 'checkdoc
    (setq checkdoc-proper-noun-list nil)
    (setq checkdoc-proper-noun-regexp
          (concat "\\_<" (regexp-opt checkdoc-proper-noun-list t)
                  "\\(\\_>\\|[.!?][ \t\n\"]\\)")))
  (setq-default checkdoc-package-keywords-flag nil))

(provide 'init-lang-elisp)

;;; init-lang-elisp.el ends here
