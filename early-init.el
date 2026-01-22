;; early-init.el

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; --- UI SETTINGS (before frame creation) ---
;; Disable startup screen
(setq inhibit-startup-message t)

;; Disable menu bar, tool bar, scroll bar (before frame is created)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (set-fringe-mode 10))

;; Disable bell sound
(setq visible-bell t
      ring-bell-function #'ignore)

;; --- BACKUP & AUTO-SAVE CONFIGURATION ---
;; Centralize all backup and auto-save files in ~/.emacs.d/
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

;; Backup behavior
(setq vc-make-backup-files nil)     ; Don't backup files controlled by Git/Version Control
(setq version-control t)              ; Use number suffixes for backups
(setq kept-new-versions 10)           ; Keep 10 newest backups
(setq kept-old-versions 0)            ; Delete old versions
(setq delete-old-versions t)          ; Silent delete
(setq backup-by-copying t)            ; Copying is safer than renaming

;; Create directories if they don't exist
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (auto-dir (expand-file-name "auto-save/" user-emacs-directory))
      (auto-list-dir (expand-file-name "auto-save-list/" user-emacs-directory)))
  (dolist (dir (list backup-dir auto-dir auto-list-dir))
    (unless (file-exists-p dir)
      (make-directory dir t))))

;; --- EDITOR PREFERENCES ---
(setq display-line-numbers-type 'relative) ; Relative line numbers

;; Use y/n instead of yes/no for faster confirmation
(setq use-short-answers t)

;; --- ENCODING & LINE ENDINGS ---
;; Set default coding system to UTF-8 with Unix (LF) line endings
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Set buffer-file-coding-system for new files
(setq-default buffer-file-coding-system 'utf-8-unix)

;; --- PERFORMANCE ---
;; Disable package.el (using elpaca instead)
(setq package-enable-at-startup nil)

;; Create directories if they don't exist
