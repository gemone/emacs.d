;;; custom-example.el --- template for the (gitignored) custom.el -*- lexical-binding: t -*-

;;; Commentary:

;;
;; Copy this file to `custom.el' (which IS gitignored) and edit to taste.
;; `init.el' loads `custom.el' via `custom-file'; THIS file is never
;; loaded — it only declares what may be configured there.

;;; Code:

;; `my/install-prog-modes' — opt-in list of prog-mode languages whose
;; packages elpaca should install/load (default nil = none).  Each gated
;; language in init.el uses `:if (memq 'SYM my/install-prog-modes)'.
;;
;; Gated symbols (add the ones you use):
;;   `zig'          — zig-mode
;;   `common-lisp'  — slime (sbcl)
;;   `java'         — eglot-java + dape + java-server (full Java stack)
;;   `typescript'   — web-mode (Angular / TS / HTML frontend)
;;   `markdown'     — markdown-mode + edit-indirect
;;
;; Always enabled (shared infra / core, NOT in the list):
;;   eglot, treesit-auto, prog-mode, eldoc-box, elisp-mode.
;;   Python needs no Emacs package — it runs via eglot + treesit-auto +
;;   the external `rass'/`ty'/`ruff' CLIs.
;;
;; Uncomment and list the languages you actually use:
;; (setq my/install-prog-modes '(zig common-lisp java typescript markdown))

;; HTTP/HTTPS proxy (elpaca and other network packages honour this):
;; (custom-set-variables
;;  '(url-proxy-services
;;    '(("http"     . "127.0.0.1:7890")
;;      ("https"    . "127.0.0.1:7890")
;;      ("no_proxy" . "^\\(localhost\\|192\\.168\\..*\\|10\\..*\\)"))))

;;; custom-example.el ends here
