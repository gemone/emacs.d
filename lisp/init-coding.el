;;; init-coding.el --- line numbers, transient, magit -*- lexical-binding: t; -*-

;;; Commentary:
;; Coding-display and version-control helpers: relative line numbers in
;; `prog-mode' (with a big-buffer fallback), `transient' (magit's UI
;; library) and `magit' itself.

;;; Code:
;;; Coding
;; Relative line numbers in prog-mode, with a big-file fallback to
;; absolute numbers (relative numbering re-renders on every cursor move).
(use-package prog-mode
  :ensure nil
  :bind (:map prog-mode-map
         ("C-c l" . my/cycle-line-numbers))
  :hook (prog-mode . my/prog-mode-line-numbers-setup)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-current-absolute t)
  (display-line-numbers-grow-only t)
  :config
  (defun my/prog-mode-line-numbers-setup ()
    "Enable line numbers: relative normally, absolute in large buffers."
    (display-line-numbers-mode 1)
    (setq-local display-line-numbers
                (if (> (count-lines (point-min) (point-max)) 5000)
                    'absolute
                  'relative)))
  (defun my/cycle-line-numbers ()
    "Cycle line-number style: relative -> absolute -> off."
    (interactive)
    (cond
     ((eq display-line-numbers 'relative)
      (setq-local display-line-numbers 'absolute)
      (message "Line numbers: absolute"))
     ((eq display-line-numbers 'absolute)
      (display-line-numbers-mode -1)
      (message "Line numbers: off"))
     (t
      (display-line-numbers-mode 1)
      (setq-local display-line-numbers 'relative)
      (message "Line numbers: relative")))))

(use-package transient
  :ensure t
  ;; Only magit (and friends) need transient; do not load it at startup.
  :defer t
  :custom
  ;; Transient (magit) history is persistent state
  (transient-history-file (expand-file-name "transient/history.el" my/state-dir)))

;; git version
;; magit-auto-revert-mode is on by default, and magit auto-detects the git
;; executable itself, so no :hook/:init magic is needed here.
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-file-dispatch))
  :custom
  (magit-status-sections-hook
   '(
     magit-insert-error-header
     magit-insert-diff-filter-header
     magit-insert-head-branch-header
     magit-insert-upstream-branch-header
     magit-insert-push-branch-header
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     ))
  ;; VC checks on file visit are expensive on Windows (each backend probe
  ;; spawns a process); skip them there.  magit talks to git directly, so
  ;; it is unaffected.
  (vc-handled-backends (if (eq system-type 'windows-nt) nil '(Git))))

(provide 'init-coding)

;;; init-coding.el ends here
