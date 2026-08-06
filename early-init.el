;;; early-init.el --- pre-load setup -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

;; Never byte-compile the user init files: with Elpaca, compiling init.el
;; bakes in elpaca internals from the compile session, and a stale
;; init.elc breaks startup (e.g. "elpaca--expand-declaration is void").
;; Remove any leftovers so Emacs always loads the source files.
(dolist (elc (list (expand-file-name "init.elc" user-emacs-directory)
                   (expand-file-name "early-init.elc" user-emacs-directory)))
  (when (file-exists-p elc)
    (delete-file elc)))

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
