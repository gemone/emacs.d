;;; init-lang-markdown.el --- Markdown editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown via `markdown-mode' 2.8+ (MELPA): native font-lock in code
;; blocks, and dedicated indirect-buffer editing with `C-c '' (needs
;; `edit-indirect').  Loaded only when `markdown' is in
;; `my/install-prog-modes' — `init.el' gates this module.

;;; Code:
;;; --- Markdown: code block editing ---
;; markdown-mode 2.8+ (MELPA).  Code blocks get the language's major mode:
;; native font-lock in place, and a dedicated indirect buffer for editing via
;; `C-c '' (`markdown-edit-code-block', needs `edit-indirect').  The mode is
;; picked by `markdown-get-lang-mode': explicit `markdown-code-lang-modes'
;; first, then *-ts-mode when the tree-sitter grammar is available, else
;; plain *-mode.
(use-package markdown-mode
  :ensure t
  ;; markdown-mode's autoloads handle .md/.markdown on file open.
  :defer t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  ;; "ts"/"js" grammars are named "typescript"/"javascript", so the ts-modes
  ;; can't be inferred from the fence language.
  (dolist (pair '(("ts" . typescript-ts-mode)
                  ("js" . js-ts-mode)))
    (add-to-list 'markdown-code-lang-modes pair)))

(use-package edit-indirect
  :ensure t
  :after markdown-mode)

(provide 'init-lang-markdown)

;;; init-lang-markdown.el ends here
