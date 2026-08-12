;;; init-lang-web-basic.el --- basic web/TS: web-mode + TS/HTML LSP -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic web editing: `web-mode' for mixed-content templates, LSP wiring
;; for the built-in typescript-ts-mode / tsx-ts-mode, and the generic
;; HTML/web LSP contact.  Loaded only when `web-basic' is in
;; `my/install-prog-modes' — `init.el' gates this module.

;;; Code:
;; The LSP contact dispatchers below consult the extensible hooks
;; `my/frontend-ts-contacts' / `my/frontend-web-contacts' (defined in
;; `init-prog-modes', extendable from custom.el); framework modules such
;; as `init-lang-web-angular' push project-aware resolvers onto them.
;; The first resolver returning non-nil wins; otherwise the stock server
;; is used.

(defun my/ts-ls-contact (&optional _interactive)
  "Return the TypeScript LSP contact for the current project.
Framework-aware via `my/frontend-ts-contacts'; defaults to
`typescript-language-server'."
  (or (run-hook-with-args-until-success 'my/frontend-ts-contacts)
      '("typescript-language-server" "--stdio")))

(defun my/web-ls-contact (&optional _interactive)
  "Return the HTML/web LSP contact for the current buffer.
Framework-aware via `my/frontend-web-contacts'; defaults to
`vscode-html-language-server'."
  (or (run-hook-with-args-until-success 'my/frontend-web-contacts)
      (eglot-alternatives
       '(("vscode-html-language-server" "--stdio")
         ("html-languageserver" "--stdio")))))

;;; --- TypeScript (built-in typescript-ts-mode / tsx-ts-mode) ---
;; No package is installed; only the LSP wiring.  Registered via
;; `with-eval-after-load' so it is present before the first connect.
;; typescript-ts-mode / tsx-ts-mode / html-ts-mode are built-in (Emacs
;; 29+) and autoloaded, so plain symbol references suffice.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(((typescript-ts-mode :language-id "typescript")
                  (typescript-mode  :language-id "typescript")
                  (tsx-ts-mode      :language-id "typescriptreact"))
                 . my/ts-ls-contact)))

;;; --- web-mode ---
;; web-mode edits mixed-content web templates: HTML with embedded CSS, JS,
;; and template engines (PHP, Django/Jinja, ERB, JSP, Mustache, EJS, ...).
;; Two core facets are made explicit and extensible here:
;;   * `my/web-mode-auto-mode'          -- file extensions that open in web-mode
;;   * `web-mode-enable-engine-detection' -- detect the template engine
;; Extend `my/web-mode-auto-mode' in custom.el to add file types, e.g.
;;   (add-to-list 'my/web-mode-auto-mode "\\.twig\\'")
;; Plain .html is intentionally NOT in the list so html-ts-mode
;; (treesit-auto) keeps handling it; add it there if you prefer web-mode.

;; Wire the extensions into `auto-mode-alist' at init (independent of when
;; web-mode loads), so opening one of these files autoloads web-mode.
(dolist (re my/web-mode-auto-mode)
  (add-to-list 'auto-mode-alist (cons re 'web-mode)))

(use-package web-mode
  :ensure t
  ;; auto-mode-alist wiring above autoloads web-mode on file open.
  :defer t
  :custom
  ;; Indentation (2 spaces; matches typical frontend style)
  (web-mode-markup-indent-offset 2)    ; HTML / tags
  (web-mode-css-indent-offset    2)    ; <style> blocks
  (web-mode-code-indent-offset   2)    ; embedded JS / template code
  (web-mode-script-padding       2)    ; left padding inside <script>
  (web-mode-style-padding        2)    ; left padding inside <style>
  (web-mode-block-padding        2)    ; padding around template control blocks
  ;; Auto-editing (core ergonomics)
  (web-mode-enable-auto-closing     t) ; close tags / brackets
  (web-mode-enable-auto-pairing     t) ; match delimiters
  (web-mode-enable-auto-quoting     t) ; quote attributes automatically
  (web-mode-enable-auto-opening     t) ; expand paired tags on split-line
  (web-mode-enable-auto-indentation t)
  ;; Engine detection: web-mode's core feature -- detect the template engine
  ;; (django, erb, jsp, php, mustache, ...) from file content so the right
  ;; block delimiters and fontification apply.  Override per file type with
  ;; `web-mode-engines-alist'.
  (web-mode-enable-engine-detection t))

;;; --- HTML / web LSP ---
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(((html-mode    :language-id "html")
                  (html-ts-mode :language-id "html")
                  (web-mode     :language-id "html"))
                 . my/web-ls-contact)))

(provide 'init-lang-web-basic)

;;; init-lang-web-basic.el ends here
