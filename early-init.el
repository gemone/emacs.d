;;; early-init.el --- pre-load setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings applied before the main init file loads: package bootstrap is
;; deferred to `init.el', GC is deferred so startup stays fast, and UTF-8
;; is configured as the default coding system.

;;; Code:

;;; GC -- defer collection until after startup
;; Emacs 30's default threshold (80 MiB) is already large, but package
;; loading and native compilation during init can still trigger many
;; collections.  Hold a much larger threshold (plus a high cons
;; percentage) for the whole startup path; `init.el' restores sane values
;; once `emacs-startup-hook' runs and reclaims the garbage at first idle.
(setq gc-cons-threshold (* 512 1024 1024)) ; 512 MiB during startup
(setq gc-cons-percentage 0.6)

;; Prefer the newest source when both .el and .elc exist, so stale
;; byte-compiled config files can never shadow current init/early-init.
(setq load-prefer-newer t)

;; Skip `file-name-handler-alist' during startup: tramp & co. handlers
;; are not needed while loading config/packages, and every path operation
;; otherwise pays a handler lookup.  Restored after `emacs-startup-hook'.
(defvar my/file-name-handler-alist-orig file-name-handler-alist
  "Saved `file-name-handler-alist' value, restored after startup.")
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist
                           my/file-name-handler-alist-orig)))

(setq package-enable-at-startup nil)

;; CODEC -- utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")

;; try codec
(setq coding-system-priority-list
      '(utf-8
        gbk
        gb18030
        big5
        cp936
        euc-tw
        shift-jis
        euc-jp
        iso-2022-jp
        koi8-r
        windows-1251
        windows-1252
        iso-8859-1
        iso-8859-15
        undecided))

(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (setenv "PYTHONIOENCODING" "utf-8")
  (add-to-list 'process-coding-system-alist '("python" . utf-8)))

(provide 'early-init)
;;; early-init.el ends here
