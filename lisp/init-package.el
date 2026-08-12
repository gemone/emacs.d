;;; init-package.el --- package manager bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Elpaca bootstrap and the use-package bridge.  This module MUST be
;; loaded before any `use-package' form: it installs Elpaca on first run
;; (cloning the repo into `elpaca/', building it and generating
;; autoloads) and enables `elpaca-use-package-mode' so every
;; `:ensure t' / `:ensure PKG' is satisfied from the Elpaca queue.
;;
;; Windows notes: symbolic links are replaced by copies
;; (`elpaca-no-symlink-mode') and a couple of slow file operations are
;; disabled.  Loaded from `init.el' as `init-package'.

;;; Code:

;;; Package Manager
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(when (eq system-type 'windows-nt)
  ;; Windows: build by copying instead of symbolic links. Must be enabled
  ;; before the build queue is processed. elpaca-no-symlink-mode is an
  ;; autoload, so calling it here auto-loads elpaca.
  (elpaca-no-symlink-mode 1)
  ;; Windows-specific file-open speedups:
  ;; - file locks add a blocking round-trip on slow/network drives
  ;; - full attribute lookups are comparatively expensive on NTFS
  (setq create-lockfiles nil)
  (setq w32-get-true-file-attributes nil))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(provide 'init-package)

;;; init-package.el ends here
