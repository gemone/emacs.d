;;; custom-example.el --- template for the (gitignored) custom.el -*- lexical-binding: t -*-

;;; Commentary:

;;
;; Copy this file to `custom.el' (which IS gitignored) and edit to taste.
;; `lisp/init-prog-modes.el' loads `custom.el' via `custom-file'; THIS
;; file is never loaded — it only declares what may be configured there.

;;; Code:

;; `my/install-prog-modes' — opt-in list of prog-mode languages whose
;; packages elpaca should install/load (default nil = none).  Each gated
;; language in `lisp/init-lang-*.el' uses `:if (memq 'SYM
;; my/install-prog-modes)'.
;;
;; Gated symbols (add the ones you use):
;;   `zig'          — zig-mode
;;   `common-lisp'  — slime (sbcl)
;;   `java'         — eglot-java + dape + java-server (full Java stack)
;;   `typescript'   — typescript-language-server for .ts/.tsx
;;   `angular'      — @angular/language-server (ngserver) for Angular projects
;;   `vue'          — Volar (@vue/language-server) for .vue SFCs
;;   `web-mode'     — web-mode + generic HTML/web LSP server (templates)
;;   `markdown'     — markdown-mode + edit-indirect
;;
;; Always enabled (shared infra / core, NOT in the list):
;;   eglot, treesit-auto, prog-mode, eldoc-box, elisp-mode.
;;   Python needs no Emacs package — it runs via eglot + treesit-auto +
;;   the external `rass'/`ty'/`ruff' CLIs.
;;
;; Uncomment and list the languages you actually use:
;; (setq my/install-prog-modes
;;       '(zig common-lisp java typescript web-mode angular vue markdown))
;;
;; Or do it interactively:  M-x my/add-prog-modes
;;   (checkbox popup that writes the list here and reloads init.el to apply)

;; ---------------------------------------------------------------------------
;; Where customize writes, and the extensible `my/' variables
;; ---------------------------------------------------------------------------
;;
;; `custom-file' is set to THIS file (custom.el) in lisp/init-prog-modes.el,
;; so anything saved via `M-x customize' or `custom-set-variables' lands here.
;;
;; The variables below are defined (in lisp/init-core.el and
;; lisp/init-prog-modes.el) BEFORE custom.el is loaded, so you can extend
;; them safely right here:
;;
;; `my/frontend-ts-contacts'  -- abnormal hook of TS LSP resolvers.  Each
;;   function takes no args and returns an eglot contact (command list) for
;;   the current project, or nil; the first non-nil wins, else
;;   `typescript-language-server' is used.  Example:
;;     (add-hook 'my/frontend-ts-contacts
;;               (defun my/foo-ts ()
;;                 (when (my/foo-project-p) '("foo-ts-ls" "--stdio"))))
;;
;; `my/frontend-web-contacts' -- abnormal hook of HTML/web LSP resolvers;
;;   same contract, consulted for html / html-ts / web-mode buffers (default
;;   `vscode-html-language-server').  The `angular' and `vue' blocks
;;   register their resolvers here automatically.
;;
;; `my/web-mode-auto-mode'    -- list of `auto-mode-alist' regexps opened in
;;   web-mode.  Add a file type, e.g. Twig templates:
;;     (add-to-list 'my/web-mode-auto-mode "\\.twig\\'")
;;   Plain .html is NOT included (html-ts-mode handles it); add it here if
;;   you want web-mode for HTML.

;; HTTP/HTTPS proxy (elpaca and other network packages honour this):
;; (custom-set-variables
;;  '(url-proxy-services
;;    '(("http"     . "127.0.0.1:7890")
;;      ("https"    . "127.0.0.1:7890")
;;      ("no_proxy" . "^\\(localhost\\|192\\.168\\..*\\|10\\..*\\)"))))

;;; custom-example.el ends here
