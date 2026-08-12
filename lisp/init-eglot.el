;;; init-eglot.el --- LSP client (eglot), eldoc-box, python LSP -*- lexical-binding: t; -*-

;;; Commentary:
;; The LSP client: built-in `eglot' (auto-start in prog-mode, inlay
;; hints, format-on-save, code-action keybindings), `eldoc-box' for
;; hover documentation in a childframe, and the Python LSP server wiring
;; (`rass' merging ty + ruff, with fallbacks).  Language-specific LSP
;; setups for Java / TypeScript / Angular / Vue / web-mode live in their
;; own `init-lang-*' modules.

;;; Code:
;;; Eglot (LSP client, built-in since Emacs 29)
(use-package eglot
  :ensure nil
  ;; LSP servers (rass/ty/ruff/ngserver/jdtls) are resolved through the
  ;; system PATH, which Emacs inherits at startup.  Make sure the install
  ;; directory (~/.local/bin, i.e. %USERPROFILE%\.local\bin on Windows) is
  ;; part of the OS PATH, then restart Emacs (or the daemon) so the new
  ;; environment is picked up.
  :hook ((prog-mode . my/eglot-maybe-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (defun my/eglot-maybe-ensure ()
    "Start eglot in `prog-mode' buffers, except the Lisp modes."
    (unless (memq major-mode '(emacs-lisp-mode common-lisp-mode))
      (eglot-ensure)))
  (defun my/eglot-format-on-save ()
    "Format the buffer with eglot when saving, if managed by it.
Named function so `add-hook' dedups it across `my/reload-config' runs."
    (when (eglot-managed-p) (eglot-format)))
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  (add-hook 'before-save-hook #'my/eglot-format-on-save)
  :bind (:map eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-action-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format)
         ;; Meow normal-state g-prefix: gd / gi / gr (extends my/meow-g-prefix-map)
         (:map meow-normal-state-keymap
               ("g d" . xref-find-definitions)
               ("g i" . xref-find-implementations)
               ("g r" . xref-find-references))))

;;; Eldoc documentation in a childframe (eglot hover docs)
(use-package eldoc-box
  :ensure t
  :demand t
  :preface
  ;; Two problems with the stock `K' flow:
  ;; - pressing `K' again while the childframe is visible moves input focus
  ;;   into the childframe; quitting from inside it can leave the main
  ;;   frame without input focus (the cursor disappears).
  ;; - in the doc buffer `meow-global-mode' shadows the `q' that eldoc-box
  ;;   binds to leave.
  ;; Fix: bind `K' to a toggle that never focuses the childframe (second
  ;; `K' just hides it), and keep `q'/`C-g' escapes in the doc buffer for
  ;; stray clicks that do land inside the frame.
  (defun my/meow-eldoc-help-at-point ()
    "Show the eldoc doc childframe, or hide it if already visible.
Unlike `eldoc-box-help-at-point', this never moves input focus into the
childframe, so hiding it always returns to the source buffer."
    (interactive)
    (if (eldoc-box--frame-visible-p)
        (eldoc-box-quit-frame)
      (eldoc-box-help-at-point)))
  (defun my/eldoc-box-doc-buffer-setup (_orig)
    "Disable meow in the doc childframe and bind `q'/`C-g' to quit it."
    (meow-mode -1)
    (local-set-key (kbd "q") #'eldoc-box-quit-frame)
    (local-set-key (kbd "C-g") #'eldoc-box-quit-frame))
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
        ;; eldoc-box is not loaded yet, so `use-package' would append
        ;; "-hook" to an unbound symbol; write the hook without the suffix
        ;; to land on `eldoc-box-buffer-setup-hook'.
        (eldoc-box-buffer-setup . my/eldoc-box-doc-buffer-setup)
  :custom
  (eldoc-box-max-pixel-width 700)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t))

;;; --- Eglot: language-specific config ---
;; The functions below (e.g. `eglot-alternatives') only exist after eglot
;; is loaded, so this runs afterwards.

;; Python: ty (type checking) + ruff (lint/format)
;; Eglot can only connect one LSP server per buffer, so rassumfrassum (rass)
;; merges the two servers into one stdio connection: `rass python' is
;; equivalent to `rass -- ty server -- ruff server'.
;; Install: uv tool install rassumfrassum ty ruff (binaries in ~/.local/bin)
;; ty/ruff options live in the project's pyproject.toml ([tool.ty] /
;; [tool.ruff]).
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 .
                 ,(eglot-alternatives
                   '(("rass" "python")            ; ty + ruff (recommended)
                     ("ty" "server")              ; ty only
                     ("ruff" "server")            ; ruff only
                     ("basedpyright-langserver" "--stdio"))))))

(provide 'init-eglot)

;;; init-eglot.el ends here
