;;; init-project.el --- projectile and treemacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Project management: projectile (project root, file switching, idle
;; activation) and treemacs (file tree, tied into projectile).

;;; Code:
;;; Project management
(use-package projectile
  :ensure t
  ;; 17k-line package: don't load it during startup at all; the idle
  ;; timer enables the mode 3 s after Emacs is idle, and autoloads cover
  ;; earlier `M-x projectile-*' use.  `C-c p' bindings appear once loaded.
  :defer t
  :init
  (defun my/projectile-lazy-init ()
    "Enable `projectile-mode' after idle, skipping active minibuffers.
Enabling projectile-mode while a minibuffer (completion) is live triggers
\"Attempt to select inactive minibuffer window\", so reschedule instead."
    (if (active-minibuffer-window)
        (run-with-idle-timer 2 nil #'my/projectile-lazy-init)
      (projectile-mode 1)))
  (run-with-idle-timer 3 nil #'my/projectile-lazy-init)
  :custom
  (projectile-switch-project-action #'projectile-find-file)
  (projectile-completion-system 'default)
  ;; Projectile state files live in var/state (gitignored), not the root
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" my/state-dir))
  (projectile-frecency-file
   (expand-file-name "projectile-frecency.eld" my/state-dir)))

(use-package treemacs
  :ensure t
  :defer t
  :custom
  ;; Persist treemacs state in var/state (gitignored)
  (treemacs-persist-file (expand-file-name "treemacs-persist" my/state-dir))
  :config
  ;; Collapse chains of single-child directories (a/b/c/... with only one
  ;; subdirectory each) into one node, up to this many levels at once.
  ;; treemacs resets this at load time, so set it here to keep it applied.
  ;; Note: requires Python (used for the async collapse scan); raise the
  ;; cap if you want deeper chains collapsed.
  (setq treemacs-collapse-dirs 5)
  :bind (("C-c t" . treemacs)
         ("C-c T" . treemacs-select-window)))

(use-package treemacs-projectile
  :ensure t
  :defer t
  :after (treemacs projectile)
  :config
  ;; Visiting a file in a projectile project auto-shows it in treemacs.
  (treemacs-project-follow-mode))

(provide 'init-project)

;;; init-project.el ends here
