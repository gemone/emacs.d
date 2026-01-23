;;; terminal.el --- Terminal configuration for Emacs -*- lexical-binding: t; -*-

;; Author: Muk
;; Version: 1.0.0
;; Keywords: convenience, terminal

;;; Commentary:

;; This file configures EAT (Emulate A Terminal) for Emacs.
;; EAT is a fast, pure Emacs Lisp terminal emulator.
;;
;; Key Features:
;; - Fast terminal emulation without external C libraries
;; - Full ANSI color support
;; - Good integration with Evil mode
;; - Multi-terminal management
;;
;; Installation:
;; EAT is available on ELPA. No external dependencies required.
;;
;; Keybindings:
;; SPC o t - Toggle terminal (hide if visible, create/show if hidden)
;; SPC o T - Always create new terminal in bottom window
;; C-c C-z - Switch to terminal mode (in terminal buffer)

;;; Code:

;;; ============================================================
;;; EAT TERMINAL CONFIGURATION
;;; ============================================================

(use-package eat
  :ensure t
  :custom
  ;; EAT settings
  (eat-kill-buffer-on-exit t)      ; Kill buffer when shell exits
  (eat-enable-shell-prompt-annotation t)  ; Better prompt handling
  (eat-enable-directory-tracking t) ; Track directory changes
  (eat-term-name "xterm-256color") ; Fix terminal type issue
  :general
  ;; Terminal keybindings (SPC o ...)
  (gemo/leader-keys
    "o"  '(:ignore t :hint "open")
    "ot" '(gemo/toggle-term :hint "Toggle terminal (bottom)")
    "oT" '(gemo/new-terminal :hint "New terminal (bottom)")))

;;; ============================================================
;;; TERMINAL HELPER FUNCTIONS
;;; ============================================================

(defun gemo--find-terminal-window ()
  "Find any visible terminal window."
  (catch 'found
    (dolist (win (window-list))
      (with-current-buffer (window-buffer win)
        (when (derived-mode-p 'eat-mode)
          (throw 'found win))))
    nil))

;;;###autoload
(defun gemo/toggle-term ()
  "Toggle EAT terminal in bottom window.
If a terminal is visible, hide it. Otherwise, create/show one."
  (interactive)
  (let ((term-win (gemo--find-terminal-window)))
    (if term-win
        ;; Terminal is visible, hide it
        (delete-window term-win)
      ;; No terminal visible, create one
      (select-window (split-window-below))
      (eat (getenv "SHELL"))
      (balance-windows))))

;;;###autoload
(defun gemo/new-terminal ()
  "Always create a new EAT terminal in bottom window."
  (interactive)
  (select-window (split-window-below))
  (eat (getenv "SHELL"))
  (balance-windows))

(provide 'terminal)

;;; terminal.el ends here
