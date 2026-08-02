;;; init.el --- user init -*- lexical-binding: t; -*-
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
;; Windows: build by copying instead of symbolic links. Must be enabled
;; before the build queue is processed. elpaca-no-symlink-mode is an
;; autoload, so calling it here auto-loads elpaca.
(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode 1))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;; Basic emacs config
(use-package emacs :ensure nil
  :custom
  (ring-bell-function #'ignore)
  (initial-frame-alist '((fullscreen . maximized)))
  (inhibit-startup-screen t)
  
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

    ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :config
  (set-frame-parameter nil 'alpha-background 95)

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (context-menu-mode t)

  (show-paren-mode t))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)

;;; theme
(use-package catppuccin-theme
  :ensure t
  :demand t
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (catppuccin-reload))

;;; font config
(use-package fontaine
  :ensure t
  :demand t
  :config
  ;; Maple Mono ships as SEPARATE families: NF has Nerd Font icons (PUA),
  ;; CN has CJK glyphs -- no NF-CN combo exists. Primary font = NF so
  ;; mode-line/completion icons render natively; CJK is wired via fontset to
  ;; the CN family below. Fallback list tried in order when not installed;
  ;; its car MUST match the family set via the fontaine presets below.
  (setq face-font-family-alternatives
        '(("Maple Mono NF"
           "CaskaydiaCove Nerd Font Mono"
           "Cascadia Code"
           "JetBrains Mono Nerd Font Mono"
           "Iosevka Nerd Font Mono"
           "DejaVu Sans Mono"
           "Monospace")))
  ;; CJK: Maple Mono CN first (shares latin metrics with NF, keeps columns
  ;; aligned). font-spec + explicit "fontset-default" is reliable on pgtk;
  ;; NAME=t with a bare family string often silently fails there.
  ;; ponytail: 'han only; add 'kana/'cjk-misc if JP/KR glyphs are needed.
  (dolist (f '("Maple Mono CN" "LXGW WenKai" "Sarasa Mono SC" "WenQuanYi Micro Hei Mono"))
    (set-fontset-font "fontset-default" 'han (font-spec :family f) nil 'append))
  (setq fontaine-presets
        '((regular :default-height 130)
          (large   :default-height 160)
          (t       :default-family "Maple Mono NF"
                   :default-weight regular
                   :fixed-pitch-family "Maple Mono NF"
                   :variable-pitch-family "Maple Mono NF"
                   :bold-weight semibold
                   :italic-slant italic
                   :line-spacing nil)))
  ;; fontaine-mode only persists the last preset across restarts; it does
  ;; NOT apply fonts. fontaine-set-preset is what actually sets faces.
  (fontaine-mode 1)
  (fontaine-set-preset 'regular))

;; Pixel-perfect vertical alignment for variable-pitch/CJK columns.
(use-package valign
  :ensure t
  ;; valign only does anything in org buffers (pixel-perfect table alignment,
  ;; incl. CJK 2x-width glyphs); the old prog-mode hook was a no-op.
  :hook (org-mode . valign-mode)
  :config
  ;; Maple Mono NF CN / CJK fallbacks are true 2x-width, so valign's default
  ;; width table already matches; no need to add custom entry.
  (setq valign-fancy-bar nil))


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
	("e" . end-of-buffer))
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

  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
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
   '("-" . negative-argument)
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
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("q" . meow-quit)
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

;;; Completion
;; MiniBuff
(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  :init
  (vertico-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t))

;; For Code
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-mouse-mode)
  (corfu-popupinfo-mode))


;;; Coding
(use-package transient
  :ensure t)
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
  (vc-handled-backends '(Git)))

;; ts
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

;; LLM coding agents in Emacs. Independent, pick one per task.
;; Requires `codex` and `pi` CLIs on PATH.

;; Codex: native client for `codex app-server`. Open via M-x codex-ide-menu.
(use-package codex-ide
  :ensure (:host github :repo "dgillis/emacs-codex-ide"))

;; Pi: frontend for the `pi` CLI. Open via M-x pi-coding-agent.
(use-package pi-coding-agent
  :ensure t)

;; Ghostel: fast terminal emulator using libghostty-vt.
;; Requires dynamic module support (module-file-suffix non-nil).
;; Native binary auto-downloads on first use. Open via M-x ghostel.
(use-package ghostel
  :ensure t
  :bind ("C-x m" . ghostel))

