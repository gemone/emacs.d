;;; init-const.el --- shared constants for other modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Side-effect-free shared definitions used across the init-*.el modules:
;; mainly the runtime directory layout under `~/.emacs.d/var/'.  No hooks,
;; no use-package, no directory creation — so dependent modules can safely
;; `(eval-when-compile (require 'init-const))' it at byte/native-compile
;; time to resolve the variables (silencing "free variable" warnings)
;; without pulling in the full `init-core' configuration.
;;
;; Loaded right after `init-package' (before `init-core'), from `init.el'.

;;; Code:
(defvar my/var-dir (expand-file-name "var/" user-emacs-directory)
  "Root of all runtime state and caches (gitignored).")

(defvar my/cache-dir (expand-file-name "cache/" my/var-dir)
  "Directory for disposable caches: backups, auto-saves, eln, tree-sitter.")

(defvar my/state-dir (expand-file-name "state/" my/var-dir)
  "Directory for persistent state: savehist, eshell, transient, projectile.")

(defvar my/backup-dir (expand-file-name "backup/" my/cache-dir)
  "Directory for versioned backups (foo.el~).")

(defvar my/auto-save-dir (expand-file-name "auto-save/" my/cache-dir)
  "Directory for auto-save files (#foo.el#).")

(provide 'init-const)

;;; init-const.el ends here
