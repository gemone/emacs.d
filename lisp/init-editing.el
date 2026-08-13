;;; init-editing.el --- meow, smartparens, paren & indent visuals -*- lexical-binding: t; -*-

;;; Commentary:
;; Text-editing ergonomics:
;;   - meow (modal editing), with its window/diagnostic leader bindings and
;;     the C-f/C-b/C-d/C-u/C-w rebinds required to keep meow's simulated
;;     keypresses consistent;
;;   - smartparens for auto-matching delimiters and structured editing;
;;   - indent-bars and rainbow-delimiters for nesting visuals.

;;; Code:
;; These are defined in `flymake' (built-in, lazily loaded); declare them
;; so byte/native-compilation of this file knows about them.
(declare-function flymake-show-buffer-diagnostics "flymake")
(declare-function flymake--diagnostics-buffer-name "flymake")

;;; Meow Editor
(use-package meow
  :ensure t
  :demand t
  :init
  (meow-global-mode 1)
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :bind
  (:map meow-insert-state-keymap
	("C-w" . meow-backward-kill-symbol)
	("C-h" . meow-backward-delete))
  (:map meow-normal-state-keymap
	:prefix "g"
	:prefix-map my/meow-g-prefix-map
	("g" . beginning-of-buffer)
	("a" . back-to-indentation)
	("l" . end-of-line)
	("e" . end-of-buffer)
	("n" . flymake-goto-next-error)
	("p" . flymake-goto-prev-error))
  (:map meow-normal-state-keymap
        :prefix "C-w"
        :prefix-map my/meow-window-map
        ("h" . windmove-left)
        ("j" . windmove-down)
        ("k" . windmove-up)
        ("l" . windmove-right)
        ("o" . delete-other-windows)
        ("v" . split-window-right)
        ("s" . split-window-below)
        ("w" . other-window)
        ("q" . delete-window))
  :config
  ;; With C-w used as a window prefix, the C-w (kill-region) that meow-kill
  ;; simulates internally would break. Per the meow docs, move kill-region
  ;; to C-M-w and update meow--kbd-kill-region accordingly.
  (meow-normal-define-key
   '("C-M-w" . kill-region))
  (setq meow--kbd-kill-region "C-M-w")

  ;; Vim-style paging in NORMAL state:
  ;;   C-f / C-b : full page down / up
  ;;   C-d / C-u : half  page down / up
  ;;
  ;; meow simulates C-f/C-b/C-d internally (forward-char, backward-char,
  ;; delete-char) by looking the keys up in the current keymap, so rebinding
  ;; them here would break h/l/d.  Point the kbd macros at the command
  ;; symbols directly (meow--execute-kbd-macro accepts symbols) to free the
  ;; keys; same pattern as the C-w -> C-M-w move above.
  (setq meow--kbd-forward-char  #'forward-char)
  (setq meow--kbd-backward-char #'backward-char)
  (setq meow--kbd-delete-char   #'delete-char)

  (defun my/meow-page-half-down (&optional arg)
    "Scroll view down by half a window ARG times (vim `C-d')."
    (interactive "p")
    (when (region-active-p) (meow-cancel-selection))
    (scroll-up-command (* arg (max 1 (/ (window-text-height) 2)))))
  (defun my/meow-page-half-up (&optional arg)
    "Scroll view up by half a window ARG times (vim `C-u')."
    (interactive "p")
    (when (region-active-p) (meow-cancel-selection))
    (scroll-down-command (* arg (max 1 (/ (window-text-height) 2)))))

  (meow-normal-define-key
   '("C-f" . meow-page-down)
   '("C-b" . meow-page-up)
   '("C-d" . my/meow-page-half-down)
   '("C-u" . my/meow-page-half-up))

  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  (defun my/meow-toggle-diagnostics ()
    "Toggle the Flymake diagnostics list for the current buffer."
    (interactive)
    (unless flymake-mode
      (user-error "Flymake mode is not enabled in the current buffer"))
    (let ((name (flymake--diagnostics-buffer-name)))
      (if-let ((win (get-buffer-window name)))
          (quit-window nil win)
        (flymake-show-buffer-diagnostics))))

  (defvar my/kmacro-recording-register nil
    "Register character currently being recorded into, or nil.")
  (defvar my/kmacro-last-register nil
    "Register character of the last `my/kmacro-call-register' (`@@').")

  (defun my/kmacro-record-toggle ()
    "Vim-style macro recording: `q<letter>' records into register <letter>.
Press `q' again to stop; the macro is then stored in that register and
can be run with `@<letter>'.  A trailing `q' stop key, if recorded, is
stripped from the macro."
    (interactive)
    (if defining-kbd-macro
        (let ((reg my/kmacro-recording-register))
          (kmacro-end-macro nil)
          (setq my/kmacro-recording-register nil)
          (when (and reg last-kbd-macro)
            ;; The stop-key `q' may have been recorded as the last event.
            (let ((macro last-kbd-macro))
              (when (and (> (length macro) 0)
                         (equal (aref macro (1- (length macro))) ?q))
                (setq macro (seq-subseq macro 0 (1- (length macro)))))
              (set-register reg macro)
              (message "Recorded macro to register %c (%d keys)"
                       reg (length macro)))))
      (let ((c (read-char "Record macro to register (a-z): ")))
        (if (and (>= c ?a) (<= c ?z))
            (progn
              (setq my/kmacro-recording-register c)
              (kmacro-start-macro nil)
              (message "Recording to register %c, press q to stop" c))
          (message "Invalid register: %c" c)))))

  (defun my/kmacro-call-register ()
    "Vim-style `@<letter>': run the macro stored in register <letter>.
`@@' reruns the last register used."
    (interactive)
    (let* ((c (read-char "Run macro from register (a-z): "))
           (reg (if (eq c ?@)
                    my/kmacro-last-register
                  (and (>= c ?a) (<= c ?z) c)))
           (macro (and reg (get-register reg))))
      (if macro
          (progn
            (setq my/kmacro-last-register reg)
            (execute-kbd-macro macro))
        (message "No macro in register %c" (or reg c)))))

  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("d" . my/meow-toggle-diagnostics)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
    '("1" . meow-expand-1)
    ;; `-' = back to the file manager / parent directory (defined in
    ;; init-dired); was negative-argument, which is rarely used in NORMAL.
    '("-" . my/dired-jump-or-parent)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . (lambda () (interactive) (end-of-line) (meow-insert)))
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . (lambda () (interactive)
	     (if (use-region-p)
		 (meow-kill)
	       (meow-delete))))
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . (lambda () (interactive) (back-to-indentation) (meow-insert)))
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . my/meow-eldoc-help-at-point)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   ;; vim-style register macros: `q<letter>' records into register
   ;; <letter> (press `q' again to stop), `@<letter>' runs it, `@@' reruns
   ;; the last one.
   '("q" . my/kmacro-record-toggle)
   '("@" . my/kmacro-call-register)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-block)
   '("V" . meow-to-block)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("/" . meow-visit)
   '("'" . repeat)
   '("<escape>" . ignore)))

;;; Smart parens
;; Auto-matching delimiters plus structured editing (wrap, slurp, barf).
;; Paired delimiters are inserted together as you type; the matching pair
;; highlight comes from `show-paren-mode' (already enabled above).
;; Keybindings live in `sp-keymap' (C-M-f/b/u/d, M-(, C-<left>/<right>...)
;; and can be trimmed if any collide with meow.
(use-package smartparens
  :ensure t
  :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  ;; Extra CJK delimiter pairs for Chinese text editing.
  (sp-pair "「" "」")
  (sp-pair "『" "』")
  (sp-pair "【" "】")
  (sp-pair "《" "》")
  (sp-pair "（" "）"))

;;; Indent guides & rainbow brackets
(use-package indent-bars
  :ensure t
  :hook ((prog-mode . indent-bars-mode))
  :custom
  ;; Color the bars by nesting depth (catppuccin-ish palette; the
  ;; option is a plist in current indent-bars, not a boolean)
  (indent-bars-color-by-depth
   '(:palette ("#f38ba8" "#fab387" "#f9e2af"
               "#a6e3a1" "#89b4fa" "#cba6f7")
     :blend 0.4))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-priority 0))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

(provide 'init-editing)

;;; init-editing.el ends here
