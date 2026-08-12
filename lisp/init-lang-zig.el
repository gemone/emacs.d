;;; init-lang-zig.el --- Zig (zig-mode) -*- lexical-binding: t; -*-

;;; Commentary:
;; Zig support via `zig-mode' (NonGNU ELPA): font-lock, indentation and
;; imenu, plus `zig fmt' formatting.  Gated on `(memq 'zig
;; my/install-prog-modes)'.  Eglot's built-in zig-mode -> zls defaults
;; work; installing zls enables LSP automatically.

;;; Code:
;;; --- Zig ---
;; zig-mode (NonGNU ELPA): provides font-lock highlighting, automatic
;; indentation and imenu, and formats via `zig fmt' (requires the zig
;; executable; see `zig-zig-bin').  Keybindings: C-c C-b build /
;; C-c C-f format / C-c C-r run / C-c C-t test.
;; Eglot's built-in zig-mode -> zls defaults work; installing zls enables
;; LSP automatically.
(use-package zig-mode
  :ensure t
  :if (memq 'zig my/install-prog-modes)
  :mode "\\.\\(zig\\|zon\\)\\'"
  :custom
  (zig-indent-offset 4)
  (zig-format-on-save t))

(provide 'init-lang-zig)

;;; init-lang-zig.el ends here
