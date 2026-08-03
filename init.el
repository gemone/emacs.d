;;; init.el --- user init -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal Emacs configuration.

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
  :preface
  ;; Shared runtime directories, referenced by the `:custom' forms below and
  ;; by other use-package blocks (savehist, transient, ...).  Semantic split
  ;; per the XDG Base Directory spec:
  ;;   state (savehist, eshell, transient history, ...)
  ;;     -> ~/.local/state/emacs/        (Linux/macOS)
  ;;     -> %LOCALAPPDATA%\emacs\state\  (Windows)
  ;;   cache (backups, auto-saves, eln, tree-sitter, ...)
  ;;     -> ~/.cache/emacs/              (Linux/macOS)
  ;;     -> %LOCALAPPDATA%\emacs\cache\  (Windows)
  (require 'xdg)
  (defvar my/cache-dir
    (if (eq system-type 'windows-nt)
        (expand-file-name "emacs/cache/"
                          (or (getenv "LOCALAPPDATA") (xdg-cache-home)))
      (expand-file-name "emacs/" (xdg-cache-home))))
  (defvar my/state-dir
    (if (eq system-type 'windows-nt)
        (expand-file-name "emacs/state/"
                          (or (getenv "LOCALAPPDATA") (xdg-cache-home)))
      (expand-file-name "emacs/" (xdg-state-home))))
  (defvar my/backup-dir (expand-file-name "backup/" my/cache-dir))
  (defvar my/auto-save-dir (expand-file-name "auto-save/" my/cache-dir))
  (dolist (dir (list my/cache-dir my/state-dir my/backup-dir my/auto-save-dir
                     (expand-file-name "auto-save-list/" my/cache-dir)
                     (expand-file-name "eshell/" my/state-dir)
                     (expand-file-name "transient/" my/state-dir)
                     (expand-file-name "tree-sitter/" my/cache-dir)
                     (expand-file-name "eln-cache/" my/cache-dir)))
    (make-directory dir t))
  (require 'treesit nil t)
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

  ;; Backups (foo.el~) -> cache/backup/
  (backup-directory-alist `(("." . ,my/backup-dir)))

  ;; Auto-saves (#foo.el#) -> cache/auto-save/, still recoverable via
  ;; `recover-file' (it derives the name the same way).
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "\\1" my/auto-save-dir) t)))
  (auto-save-list-file-prefix
   (expand-file-name "auto-save-list/.saves-" my/cache-dir))

  ;; eshell history is state, not cache
  (eshell-directory-name (expand-file-name "eshell/" my/state-dir))

  ;; Native-compiled elisp files are a pure cache
  (native-comp-eln-load-path
   (cons (expand-file-name "eln-cache/" my/cache-dir)
         (cdr native-comp-eln-load-path)))

  ;; Tree-sitter grammars are compiled caches
  (treesit-extra-load-path
   (if (boundp 'treesit-extra-load-path)
       (cons (expand-file-name "tree-sitter/" my/cache-dir)
             treesit-extra-load-path)))

  ;; Auto-revert buffers when the file on disk changes
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)

  :config
  (set-frame-parameter nil 'alpha-background 95)

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (context-menu-mode t)

  (show-paren-mode t)

  (global-auto-revert-mode 1)

  ;; Emacs 30 hard-codes the tree-sitter install dir under
  ;; `user-emacs-directory'; redirect future installs into the cache too.
  (when (fboundp 'treesit-install-language-grammar)
    (defun my/treesit-install-to-cache (orig-fun lang &optional out-dir)
      "Call ORIG-FUN, defaulting OUT-DIR to the cache grammar dir."
      (funcall orig-fun lang
               (or out-dir (expand-file-name "tree-sitter/" my/cache-dir))))
    (advice-add 'treesit-install-language-grammar
                :around #'my/treesit-install-to-cache)))

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

  ;; Vim-style paging in NORMAL state (vim 翻页):
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
  :custom
  ;; Minibuffer history is persistent state, not cache
  (savehist-file (expand-file-name "history" my/state-dir))
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


;;; IBuffer & isearch
;; ibuffer is the built-in Dired-like buffer manager (Emacs 30.2).
;; isearch-related keys (all built-in defaults):
;;   C-s / C-r   : incremental search over the buffer list
;;   M-s a C-s   : incremental search in marked buffers (ibuffer-do-isearch)
;;   M-s a C-M-s : same, regexp variant (ibuffer-do-isearch-regexp)
;;   Filtering (/ prefix): / n name  / b basename  / f filename  / m mode  / c content
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)   ; use ibuffer instead of the default list-buffers
         :map ibuffer-mode-map
         ;; Already bound by default; declared here for clarity and easy override
         ("M-s a C-s" . ibuffer-do-isearch)
         ("M-s a C-M-s" . ibuffer-do-isearch-regexp))
  :hook ((ibuffer-mode . hl-line-mode)
         (ibuffer-mode . ibuffer-auto-mode))
  :custom
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-show-empty-filter-groups nil))

;;; Coding
;; Relative line numbers in prog-mode, with a big-file fallback to
;; absolute numbers (relative numbering re-renders on every cursor move).
(use-package prog-mode
  :ensure nil
  :preface
  (defun my/disable-line-numbers ()
    "Turn off display line numbers in the current buffer."
    (display-line-numbers-mode -1))
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
  (vc-handled-backends '(Git)))

;; ts
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

;; uv/npm 全局工具目录（rass/ty/ruff/ngserver 等 LSP 服务器所在处）
;; exec-path 只管 Emacs 自己找程序；子进程（如 rass 再拉起 ty/ruff）
;; 继承的是 PATH 环境变量，所以两者都要设置。
(let* ((bin (expand-file-name "~/.local/bin"))
       (old (or (getenv "PATH") ""))
       (new (mapconcat #'identity (cons bin (parse-colon-path old)) ":")))
  (add-to-list 'exec-path bin)
  (setenv "PATH" new))

;;; Eglot (LSP client, built-in since Emacs 29)
(use-package eglot
  :ensure nil
  :hook ((prog-mode . (lambda ()
                        (unless (eq major-mode 'emacs-lisp-mode)
                          (eglot-ensure)))))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  (add-hook 'before-save-hook
            (lambda () (when (eglot-managed-p) (eglot-format))))
  :bind (:map eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-action-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format)
         ;; Meow normal-state g-prefix: gd / gi / gr (extends my/meow-g-prefix-map)
         (:map meow-normal-state-keymap
               ("g d" . xref-find-definitions)
               ("g i" . xref-find-implementations)
               ("g r" . xref-find-references))))

;;; --- Eglot: 语言特定配置 ---
;; 下面用到 `eglot-alternatives' 等函数，所以放在 eglot 加载后再执行。

;; Python: ty（类型检查）+ ruff（lint/格式化）
;; Eglot 每个 buffer 只能连一个 LSP server，所以用 rassumfrassum (rass)
;; 把两个 server 合并成一条 stdio 连接：`rass python' 等价于
;; `rass -- ty server -- ruff server'。
;; 安装：uv tool install rassumfrassum ty ruff（可执行文件在 ~/.local/bin）
;; ty/ruff 各自的配置写在项目的 pyproject.toml（[tool.ty] / [tool.ruff]）。
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 .
                 ,(eglot-alternatives
                   '(("rass" "python")            ; ty + ruff（推荐）
                     ("ty" "server")              ; 仅 ty
                     ("ruff" "server")            ; 仅 ruff
                     ("basedpyright-langserver" "--stdio"))))))

;; Java: Eclipse JDT Language Server (jdtls)
;; `-data' 指向缓存目录，避免 workspace 元数据散落到项目里
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode)
                 .
                 ("jdtls"
                  "-data" ,(expand-file-name "jdtls-workspace" my/cache-dir)))))

;;; --- Eglot: Angular / web-mode ---
;; npm 的 @angular/language-server 包提供的可执行文件叫 `ngserver'。
;; - .ts/.tsx 文件：Angular 项目用 ngserver，否则 typescript-language-server
;; - .html 模板（web-mode/html-ts-mode 等）：Eglot 每个 buffer 只能连一个
;;   LSP server，所以 Angular 项目里用 rass 把 ngserver + vscode-html-language-server
;;   + vscode-css-language-server 三个 server 合并成一条连接；
;;   普通项目回退到默认 HTML server。
;; 安装：npm install -g @angular/language-server @angular/language-service
;;       typescript typescript-language-server vscode-langservers-extracted

(defun my/angular-project-p ()
  "Return non-nil if current project is an Angular project."
  (when-let* ((project (project-current))
              (root (expand-file-name (project-root project))))
    (or (file-exists-p (expand-file-name "angular.json" root))
        (file-exists-p (expand-file-name "project.json" root)))))

(defun my/angular-probes ()
  "Return comma-separated probe paths for `ngserver'."
  (let* ((root (expand-file-name (project-root (project-current))))
         (local-nm (expand-file-name "node_modules" root))
         (global-nm (string-trim (shell-command-to-string "npm root -g")))
         (probes (delq nil
                       (list (when (file-directory-p local-nm) local-nm)
                             (unless (string-empty-p global-nm) global-nm)))))
    (mapconcat #'identity probes ",")))

(defun my/angular-ls-command ()
  "Return command for `ngserver' with probe locations."
  (list "ngserver" "--stdio"
        "--tsProbeLocations" (my/angular-probes)
        "--ngProbeLocations" (my/angular-probes)))

(defun my/angular-ts-contact (_interactive)
  "Use Angular server for Angular projects, else `typescript-language-server'."
  (if (my/angular-project-p)
      (my/angular-ls-command)
    '("typescript-language-server" "--stdio")))

(defun my/angular-web-contact (_interactive)
  "HTML/web-mode 多服务器方案.

Angular 项目里用 rass 合并 ngserver + vscode-html-language-server
+ vscode-css-language-server；普通项目回退到默认 HTML server。"
  (if (my/angular-project-p)
      (list "rass" "--"
            "ngserver" "--stdio"
            "--tsProbeLocations" (my/angular-probes)
            "--ngProbeLocations" (my/angular-probes)
            "--" "vscode-html-language-server" "--stdio"
            "--" "vscode-css-language-server" "--stdio")
    (eglot-alternatives
     '(("vscode-html-language-server" "--stdio")
       ("html-languageserver" "--stdio")))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(((typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript")
                  (tsx-ts-mode :language-id "typescriptreact"))
                 . my/angular-ts-contact))
  (add-to-list 'eglot-server-programs
               '(((html-mode :language-id "html")
                  (html-ts-mode :language-id "html")
                  (web-mode :language-id "html"))
                 . my/angular-web-contact)))

;;; Elisp 语法/静态检查（Emacs Lisp 没有 LSP server）
(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . flymake-mode)
         ;; 配置类文件只保留 checkdoc 后端：byte-compile 子进程的 load-path
         ;; 只有 "./"，看不到 Elpaca 安装的包，会产生大量"函数未定义"噪音。
         (emacs-lisp-mode . (lambda ()
                              (remove-hook 'flymake-diagnostic-functions
                                           #'elisp-flymake-byte-compile t))))
  :config
  ;; Emacs 30 的 `emacs-lisp-mode' 默认注册两个 flymake 后端：
  ;; `elisp-flymake-byte-compile'（编译错误，已在上方移除）和
  ;; `elisp-flymake-checkdoc'（文档/风格，保留）
  (setq-default checkdoc-package-keywords-flag nil))

;; LLM coding agents in Emacs. Independent, pick one per task.
;; Requires `codex` and `pi` CLIs on PATH.

;; Codex: native client for `codex app-server`. Open via M-x codex-ide-menu.
(use-package codex-ide
  :ensure (:host github :repo "dgillis/emacs-codex-ide")
  :bind (("C-c C-a" . codex-ide-menu))
  :config
  ;; IDE 面板/会话 buffer 不显示行号
  (add-hook 'codex-ide-session-mode-hook #'my/disable-line-numbers)
  (add-hook 'codex-ide-loop-mode-hook #'my/disable-line-numbers)
  (add-hook 'codex-ide-section-mode-hook #'my/disable-line-numbers)
  (add-hook 'codex-ide-log-mode-hook #'my/disable-line-numbers)
  (add-hook 'codex-ide-session-buffer-list-mode-hook #'my/disable-line-numbers))

;; Pi: frontend for the `pi` CLI. Open via M-x pi-coding-agent.
(use-package pi-coding-agent
  :ensure t
  :config
  (add-hook 'pi-coding-agent-chat-mode-hook #'my/disable-line-numbers)
  (add-hook 'pi-coding-agent-input-mode-hook #'my/disable-line-numbers))

;; Ghostel: fast terminal emulator using libghostty-vt.
;; Requires dynamic module support (module-file-suffix non-nil).
;; Native binary auto-downloads on first use. Open via M-x ghostel.
(use-package ghostel
  :ensure t
  :bind ("C-x m" . ghostel))

(provide 'init)

;;; init.el ends here
