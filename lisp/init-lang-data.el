;;; init-lang-data.el --- YAML / TOML / JSON (built-in ts modes) -*- lexical-binding: t; -*-

;;; Commentary:
;; Data-format editing on Emacs 30's built-in tree-sitter modes:
;; `yaml-ts-mode', `toml-ts-mode', `json-ts-mode'.  No packages to
;; install — treesit-auto maps the file extensions and fetches the
;; grammars on first visit (see `lisp/init-treesit.el').
;;
;; Eglot uses its built-in server defaults for JSON and YAML
;; (`vscode-json-languageserver' / `yaml-language-server'); TOML has
;; none, so `taplo' is wired here — each becomes active when the
;; server is on PATH (see `lisp/init-eglot.el').  Loaded only when
;; `data' is in `my/install-prog-modes' — `init.el' gates this module.

;;; Code:
;; --- YAML ---
;; Emacs 30.1's `yaml-ts-mode' has no indent-offset variable (it only
;; enforces `indent-tabs-mode' nil).  treesit-auto remaps yaml-mode ->
;; yaml-ts-mode, but that only helps when the yaml-mode package is
;; installed; map the extension directly so .yaml/.yml always land in
;; yaml-ts-mode (and its grammar auto-installs on first visit).
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; --- TOML ---
(with-eval-after-load 'toml-ts-mode
  (setq toml-ts-mode-indent-offset 2))

;; --- JSON ---
(with-eval-after-load 'json-ts-mode
  (setq json-ts-mode-indent-offset 2))

;; --- Eglot: TOML has no built-in server mapping; use taplo when on PATH.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((toml-ts-mode) . ("taplo" "lsp" "--stdio"))))

(provide 'init-lang-data)

;;; init-lang-data.el ends here
