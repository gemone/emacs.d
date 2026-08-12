;;; init-lang-lisp.el --- Common Lisp (SLIME) -*- lexical-binding: t; -*-

;;; Commentary:
;; Common Lisp development via SLIME over a SWANK connection to SBCL.
;; Gated on `(memq 'common-lisp my/install-prog-modes)'.  Start with
;; `M-x slime', or open a .lisp file and press C-c C-z for the REPL.

;;; Code:
;;; --- Common Lisp (SLIME) ---
;; SLIME (Superior Lisp Interaction Mode for Emacs) is the standard Common
;; Lisp development environment: REPL, inspector, debugger and
;; cross-referencing over a SWANK connection to SBCL.  Start with `M-x
;; slime', or open a .lisp file and press C-c C-z for the REPL.
(use-package slime
  :ensure t
  :if (memq 'common-lisp my/install-prog-modes)
  :mode ("\\.lisp\\'" . common-lisp-mode)
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(provide 'init-lang-lisp)

;;; init-lang-lisp.el ends here
