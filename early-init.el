;;; early-init.el --- pre-load setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings applied before the main init file loads: package bootstrap is
;; deferred to `init.el', stale byte-compiled config files are removed, and
;; UTF-8 is configured as the default coding system.

;;; Code:

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
