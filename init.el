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
  ;;
  ;; Use `concat' + `file-name-as-directory', NOT `expand-file-name':
  ;; (expand-file-name "\\1" DIR) treats "\\1" as an absolute path on
  ;; Windows (backslash is a separator there) and silently drops DIR,
  ;; collapsing the replacement to the drive root ("c:/1"), so the
  ;; autosave file ends up under "C:\\#..." and the write fails -- no
  ;; autosave/recover file is ever produced.  `concat' keeps DIR and
  ;; preserves the "\\1" backref on every platform; on POSIX the result
  ;; is identical to the old `expand-file-name' form.
  (auto-save-file-name-transforms
   `((".*" ,(concat (file-name-as-directory my/auto-save-dir) "\\1") t)))
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

)
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
  ;; Probe installed fonts at startup and pick the first available family
  ;; from each chain, so the config works on machines with only some of the
  ;; fonts installed.  Maple Mono ships as SEPARATE families: NF has Nerd
  ;; Font icons (PUA), CN has CJK glyphs; some builds also provide a
  ;; combined "NF CN".  Prefer the combined family when present, then NF
  ;; (icons for mode-line/completion), then CN / generic CJK-capable
  ;; fallbacks.  The chosen latin family is used for the fontaine presets,
  ;; and `face-font-family-alternatives' keeps the whole chain as backup.
  (defun my/font-available-p (family)
    "Return non-nil if FAMILY is installed on the current display."
    (and (display-graphic-p)
         (member family (font-family-list))))

  (defun my/select-font (families)
    "Return the first installed font in FAMILIES.
If no font can be probed (e.g. daemon/terminal at load time) or none is
installed, return the first element of FAMILIES as a safe default."
    (catch 'found
      (dolist (family families)
        (when (my/font-available-p family)
          (throw 'found family)))
      (car families)))

  (let* ((latin-chain '("Maple Mono NF CN" "Maple Mono NF" "Maple Mono CN"
                        "CaskaydiaCove Nerd Font Mono" "Cascadia Code"
                        "JetBrains Mono Nerd Font Mono" "Iosevka Nerd Font Mono"
                        "DejaVu Sans Mono" "Monospace"))
         (cjk-chain '("Maple Mono NF CN" "Maple Mono CN" "LXGW WenKai"
                      "Sarasa Mono SC" "WenQuanYi Micro Hei Mono"))
         (main-font (my/select-font latin-chain))
         (cjk-font  (my/select-font cjk-chain)))
    (setq face-font-family-alternatives (list latin-chain))
    ;; CJK primary + remaining installed candidates as glyph-level fallback.
    ;; font-spec + explicit "fontset-default" is reliable on pgtk; NAME=t
    ;; with a bare family string often silently fails there.
    (set-fontset-font "fontset-default" 'han (font-spec :family cjk-font))
    (dolist (family (cdr (member cjk-font cjk-chain)))
      (set-fontset-font "fontset-default" 'han (font-spec :family family) nil 'append))
    (setq fontaine-presets
          `((regular :default-height 130)
            (large   :default-height 160)
            (t       :default-family ,main-font
                     :default-weight regular
                     :fixed-pitch-family ,main-font
                     :variable-pitch-family ,main-font
                     :bold-weight semibold
                     :italic-slant italic
                     :line-spacing nil))))
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
	("e" . end-of-buffer)
	("n" . flymake-goto-next-error)
	("p" . flymake-goto-prev-error))
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

  ;; Vim-style paging in NORMAL state:
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

  (defun my/meow-toggle-diagnostics ()
    "Toggle the Flymake diagnostics list for the current buffer."
    (interactive)
    (unless flymake-mode
      (user-error "Flymake mode is not enabled in the current buffer"))
    (let ((name (flymake--diagnostics-buffer-name)))
      (if-let ((win (get-buffer-window name)))
          (quit-window nil win)
        (flymake-show-buffer-diagnostics))))

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
   '("d" . my/meow-toggle-diagnostics)
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
   '("K" . my/meow-eldoc-help-at-point)
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

;;; Smart parens
;; Auto-matching delimiters plus structured editing (wrap, slurp, barf).
;; Paired delimiters are inserted together as you type; the matching pair
;; highlight comes from `show-paren-mode' (already enabled above).
;; Keybindings live in `sp-keymap' (C-M-f/b/u/d, M-(, C-<left>/<right>...)
;; and can be trimmed if any collide with meow.
(use-package smartparens
  :ensure t
  :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  ;; Extra CJK delimiter pairs for Chinese text editing.
  (sp-pair "「" "」")
  (sp-pair "『" "』")
  (sp-pair "【" "】")
  (sp-pair "《" "》")
  (sp-pair "（" "）"))

;;; Indent guides & rainbow brackets
(use-package indent-bars
  :ensure t
  :hook ((prog-mode . indent-bars-mode))
  :custom
  ;; Color the bars by nesting depth (catppuccin-ish palette; the
  ;; option is a plist in current indent-bars, not a boolean)
  (indent-bars-color-by-depth
   '(:palette ("#f38ba8" "#fab387" "#f9e2af"
               "#a6e3a1" "#89b4fa" "#cba6f7")
     :blend 0.4))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-priority 0))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

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
  ;; Note: `+orderless-consult-dispatch' was removed in current Consult;
  ;; only `orderless-affix-dispatch' exists (`!' exclude, `=' literal).
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  ;; LSP candidates (eglot) are already prefix-filtered by the server, so
  ;; use `basic' matching for them; orderless would otherwise re-filter and
  ;; hide valid candidates.
  (completion-category-overrides '((eglot-capf (styles basic))
                                   (file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t))

(defun my/consult-find ()
  "Fuzzy-find files with `fd' when available, else fall back to `find'."
  (interactive)
  (call-interactively (if (executable-find "fd") #'consult-fd #'consult-find)))

(use-package consult
  :ensure t
  :bind (("C-c f" . my/consult-find)
         ("C-c F" . consult-ripgrep)
         ("C-s" . consult-line))
  :config
  ;; Use projectile roots instead of the built-in project.el.
  ;; (`consult-project-function' takes MAY-PROMPT; ignore it.)
  (setq consult-project-function
        (lambda (&rest _) (projectile-project-root))))

(use-package zoxide
  :ensure t)

(use-package consult-dir
  :ensure t
  ;; Note: C-c d is taken by dape's key prefix, so use C-c z (zoxide).
  :bind (("C-c z" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-j" . consult-dir-jump-file))
  :config
  ;; zoxide history as a consult-dir source; narrow with `z'.
  (defvar consult-dir-source-zoxide
    `(:name "Zoxide"
            :narrow ?z
            :category file
            :face consult-file
            :history file-name-history
            :enabled ,(lambda () (featurep 'zoxide))
            :items ,#'zoxide-query)
    "Zoxide directory source for `consult-dir'.")
  (add-to-list 'consult-dir-sources 'consult-dir-source-zoxide t))

;; For Code
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;; Quit (hide popup) when there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  (corfu-auto t)                 ;; Popup while typing (idle-based)
  (corfu-auto-delay 0.2)         ;; Idle delay before showing candidates
  (corfu-auto-prefix 2)          ;; Minimum prefix length for auto popup

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

;; Extra completion sources layered on top of LSP (eglot):
;; - eglot registers its capf buffer-locally, so it is tried FIRST and
;;   provides server completions for identifiers;
;; - when eglot returns nil (comments, strings, no server, non-LSP modes)
;;   these global sources fill in: file names, words in the buffer,
;;   dictionary words, and elisp code blocks.
(use-package cape
  :ensure t
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))


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

;;; Isearch
;; Built-in isearch tuning: match counter, immediate multi-match
;; highlighting, and scroll/wrap ergonomics.
(use-package isearch
  :ensure nil
  :custom
  ;; Show "N/total" while searching
  (isearch-lazy-count t)
  ;; Highlight all matches immediately (no idle delay)
  (lazy-highlight-initial-delay 0)
  ;; Allow C-v/M-v and other motion commands during isearch
  (isearch-allow-scroll t)
  ;; Wrap to the other end without pausing
  (isearch-wrap-pause 'no)
  ;; Reversing direction keeps the current search string
  (isearch-repeat-on-direction-change t)
  ;; Whitespace in the pattern matches any run of whitespace
  (search-whitespace-regexp "\\s-+")
  :config
  ;; C-o lists all matches in an Occur buffer
  (define-key isearch-mode-map (kbd "C-o") #'isearch-occur))

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
  ;; VC checks on file visit are expensive on Windows (each backend probe
  ;; spawns a process); skip them there.  magit talks to git directly, so
  ;; it is unaffected.
  (vc-handled-backends (if (eq system-type 'windows-nt) nil '(Git))))

;;; Project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :custom
  (projectile-switch-project-action #'projectile-find-file)
  (projectile-completion-system 'default))

(use-package treemacs
  :ensure t
  :defer t
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

;; ts
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  ;; Prompt to install missing grammars when visiting a file (uses the CLI
  ;; install path below); set to t for silent auto-install.
  (setq treesit-auto-install 'prompt)
  ;; zig is handled by zig-mode below; keep treesit-auto from installing the
  ;; zig grammar and remapping .zig/.zon to the built-in zig-ts-mode.
  (setq treesit-auto-langs (delq 'zig treesit-auto-langs))

  ;; Install tree-sitter grammars via the `tree-sitter' CLI
  ;; (tree-sitter-cli) instead of Emacs's built-in cc compilation:
  ;; `tree-sitter generate --abi N' regenerates parser.c at the ABI
  ;; version of this Emacs, and `tree-sitter build' handles scanners and
  ;; linking.  Output goes straight into the cache directory (already on
  ;; `treesit-extra-load-path').  Falls back to the built-in installer if
  ;; `tree-sitter' is not on PATH.
  (when (fboundp 'treesit-install-language-grammar)
    ;; `tree-sitter' is resolved through the system PATH (cargo installs it
    ;; to ~/.cargo/bin); make sure that directory is on the OS PATH and
    ;; restart Emacs (or the daemon) so the new environment is inherited.

    (defun my/treesit-cli-run (&rest args)
      "Run ARGS as a subprocess; signal `treesit-error' on failure."
      (with-temp-buffer
        (unless (eq 0 (apply #'call-process (car args) nil t nil (cdr args)))
          (signal 'treesit-error
                  (list (string-join (cons (car args) (cdr args)) " ")
                        (buffer-string))))))

    (defun my/treesit-cli-grammar-dir (repo &optional source-dir)
      "Locate the directory holding the grammar to build in REPO."
      (let* ((default-directory repo)
             (candidates (if source-dir (list source-dir ".") '(".")))
             (dir (seq-find
                   (lambda (d)
                     (or (file-exists-p (expand-file-name "grammar.js" d))
                         (file-exists-p (expand-file-name "grammar.json" d))))
                   candidates)))
        (expand-file-name (or dir (or source-dir "src")))))

    (defun my/treesit-cli-install-language-grammar (lang &optional out-dir)
      "Install grammar LANG using the `tree-sitter' CLI.
Clones the recipe repo, regenerates parser.c at the ABI version of
this Emacs, builds the shared library with `tree-sitter build' and
copies it to OUT-DIR (default the cache grammar dir)."
      (let* ((recipe (assoc lang treesit-language-source-alist))
             (url (nth 1 recipe))
             (revision (nth 2 recipe))
             (source-dir (nth 3 recipe))
             (workdir (make-temp-file "treesit-workdir" t))
             (repo (expand-file-name "repo" workdir))
             (out-dir (expand-file-name
                       (or out-dir
                           (expand-file-name "tree-sitter/" my/cache-dir))))
             (abi (or (and (fboundp 'treesit-library-abi-version)
                           (treesit-library-abi-version))
                      14))
             (lib-name (format "libtree-sitter-%s%s"
                               lang (or (car dynamic-library-suffixes) ".so"))))
        (unwind-protect
            (progn
              (unless url
                (signal 'treesit-error
                        (list "No recipe for" lang
                              "in `treesit-language-source-alist'")))
              (message "tree-sitter: cloning %s" url)
              (if revision
                  (my/treesit-cli-run "git" "clone" "--depth" "1" "--quiet"
                                      "-b" revision url repo)
                (my/treesit-cli-run "git" "clone" "--depth" "1" "--quiet"
                                    url repo))
              (let* ((grammar-dir (my/treesit-cli-grammar-dir repo source-dir))
                     (default-directory grammar-dir))
                (when (or (file-exists-p "grammar.js")
                          (file-exists-p "grammar.json"))
                  (message "tree-sitter: generating parser for %s (ABI %d)"
                           lang abi)
                  (my/treesit-cli-run "tree-sitter" "generate"
                                      "--abi" (number-to-string abi)))
                (unless (file-exists-p out-dir)
                  (make-directory out-dir t))
                (message "tree-sitter: building %s" lib-name)
                (my/treesit-cli-run "tree-sitter" "build"
                                    "-o" (expand-file-name lib-name out-dir))))
          (ignore-errors (delete-directory workdir t)))
        ;; Mirror the built-in behavior: verify the grammar loads after install.
        (pcase-let ((`(,available . ,err)
                     (treesit-language-available-p lang t)))
          (if (not available)
              (progn
                (display-warning
                 'treesit
                 (format "tree-sitter CLI install failed for %s: %s"
                         lang (mapconcat (lambda (x) (format "%s" x)) err " ")))
                t)
            (message "tree-sitter: %s installed to %s" lang out-dir)
            nil))))

    (defun my/treesit-install-async (lang)
      "Install grammar LANG in a detached Emacs batch process.
The subprocess reuses the CLI installer above, so the frontend is
never blocked while the grammar is downloaded and built."
      (let* ((emacs (expand-file-name invocation-name invocation-directory))
             (buf (get-buffer-create (format "*treesit-install-%s*" lang)))
             (target (current-buffer))
             (body
              (format
               (concat
                "(progn "
                "(require 'treesit) "
                "(defvar my/cache-dir %S) "
                "(setq treesit-language-source-alist %S) "
                "(setq treesit-extra-load-path %S) "
                "(setf (symbol-function 'my/treesit-cli-run) %S) "
                "(setf (symbol-function 'my/treesit-cli-grammar-dir) %S) "
                "(setf (symbol-function 'my/treesit-cli-install-language-grammar) %S) "
                "(let ((failed (if (executable-find \"tree-sitter\") "
                "(my/treesit-cli-install-language-grammar %S nil) "
                "(treesit-install-language-grammar %S nil)))) "
                "(kill-emacs (if (or failed (not (treesit-language-available-p %S))) 1 0))))")
               my/cache-dir
               treesit-language-source-alist
               treesit-extra-load-path
               (symbol-function 'my/treesit-cli-run)
               (symbol-function 'my/treesit-cli-grammar-dir)
               (symbol-function 'my/treesit-cli-install-language-grammar)
               lang lang lang)))
        (with-current-buffer buf (erase-buffer))
        (message "tree-sitter: installing %s grammar in background" lang)
        (make-process
         :name (format "treesit-install-%s" lang)
         :noquery t
         :buffer buf
         :command (list emacs "-Q" "--batch" "--eval" body)
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (if (zerop (process-exit-status proc))
                 (progn
                   (message "tree-sitter: %s grammar installed" lang)
                   ;; Re-enable the ts-mode in the buffer that triggered the
                   ;; install, so no manual reopen is needed.
                   (when (and (buffer-live-p target)
                              (fboundp 'treesit-auto--get-mode-recipe)
                              (fboundp 'treesit-auto--ready-p))
                     (with-current-buffer target
                       (when-let* ((recipe (treesit-auto--get-mode-recipe))
                                   (ts-mode (treesit-auto-recipe-ts-mode recipe))
                                   (grammar (treesit-auto-recipe-lang recipe))
                                   ((eq grammar lang))
                                   ((treesit-auto--ready-p ts-mode)))
                         (funcall ts-mode)
                         (message "tree-sitter: enabled %s" ts-mode)))))
               (message
                "tree-sitter: background install of %s failed; see *treesit-install-%s*"
                lang lang)))))))

    (defun my/treesit-install-via-cli (orig-fun lang &optional out-dir)
      "Install LANG in the background; fall back to ORIG-FUN when needed.
Background install is skipped for explicit interactive calls
(\`M-x treesit-install-language-grammar') or with a prefix arg."
      (if (or (treesit-language-available-p lang)
              (eq out-dir 'interactive)
              current-prefix-arg)
          (funcall orig-fun lang out-dir)
        (my/treesit-install-async lang)
        nil))

    (advice-add 'treesit-install-language-grammar
                :around #'my/treesit-install-via-cli)))

;;; --- Zig ---
;; zig-mode (NonGNU ELPA): provides font-lock highlighting, automatic
;; indentation and imenu, and formats via `zig fmt' (requires the zig
;; executable; see `zig-zig-bin').  Keybindings: C-c C-b build /
;; C-c C-f format / C-c C-r run / C-c C-t test.
;; Eglot's built-in zig-mode -> zls defaults work; installing zls enables
;; LSP automatically.
(use-package zig-mode
  :ensure t
  :mode "\\.\\(zig\\|zon\\)\\'"
  :custom
  (zig-indent-offset 4)
  (zig-format-on-save t))

;;; Eglot (LSP client, built-in since Emacs 29)
(use-package eglot
  :ensure nil
  ;; LSP servers (rass/ty/ruff/ngserver/jdtls) are resolved through the
  ;; system PATH, which Emacs inherits at startup.  Make sure the install
  ;; directory (~/.local/bin, i.e. %USERPROFILE%\.local\bin on Windows) is
  ;; part of the OS PATH, then restart Emacs (or the daemon) so the new
  ;; environment is picked up.
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

;;; Eldoc documentation in a childframe (eglot hover docs)
(use-package eldoc-box
  :ensure t
  :demand t
  :preface
  ;; Two problems with the stock `K' flow:
  ;; - pressing `K' again while the childframe is visible moves input focus
  ;;   into the childframe; quitting from inside it can leave the main
  ;;   frame without input focus (the cursor disappears).
  ;; - in the doc buffer `meow-global-mode' shadows the `q' that eldoc-box
  ;;   binds to leave.
  ;; Fix: bind `K' to a toggle that never focuses the childframe (second
  ;; `K' just hides it), and keep `q'/`C-g' escapes in the doc buffer for
  ;; stray clicks that do land inside the frame.
  (defun my/meow-eldoc-help-at-point ()
    "Show the eldoc doc childframe, or hide it if already visible.
Unlike `eldoc-box-help-at-point', this never moves input focus into the
childframe, so hiding it always returns to the source buffer."
    (interactive)
    (if (eldoc-box--frame-visible-p)
        (eldoc-box-quit-frame)
      (eldoc-box-help-at-point)))
  (defun my/eldoc-box-doc-buffer-setup (_orig)
    "Disable meow in the doc childframe and bind `q'/`C-g' to quit it."
    (meow-mode -1)
    (local-set-key (kbd "q") #'eldoc-box-quit-frame)
    (local-set-key (kbd "C-g") #'eldoc-box-quit-frame))
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
        ;; eldoc-box is not loaded yet, so `use-package' would append
        ;; "-hook" to an unbound symbol; write the hook without the suffix
        ;; to land on `eldoc-box-buffer-setup-hook'.
        (eldoc-box-buffer-setup . my/eldoc-box-doc-buffer-setup)
  :custom
  (eldoc-box-max-pixel-width 700)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t))

;;; --- Eglot: language-specific config ---
;; The functions below (e.g. `eglot-alternatives') only exist after eglot
;; is loaded, so this runs afterwards.

;; Python: ty (type checking) + ruff (lint/format)
;; Eglot can only connect one LSP server per buffer, so rassumfrassum (rass)
;; merges the two servers into one stdio connection: `rass python' is
;; equivalent to `rass -- ty server -- ruff server'.
;; Install: uv tool install rassumfrassum ty ruff (binaries in ~/.local/bin)
;; ty/ruff options live in the project's pyproject.toml ([tool.ty] /
;; [tool.ruff]).
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 .
                 ,(eglot-alternatives
                   '(("rass" "python")            ; ty + ruff (recommended)
                     ("ty" "server")              ; ty only
                     ("ruff" "server")            ; ruff only
                     ("basedpyright-langserver" "--stdio"))))))

;;; --- Java: full development environment (jdtls + eglot-java + dape + java-server) ---
;; References:
;;   - https://emacs-china.org/t/emacs-eglot-eglot-java-dape-java/30086
;;   - https://github.com/LuciusChen/java-server
;;
;; Components:
;;   - eglot-java: eglot extension for JDTLS. Downloads eclipse.jdt.ls on the
;;     first open of a .java file (upgrade manually with
;;     `M-x eglot-java-upgrade-lsp-server'), and provides project/class
;;     creation, Maven/Gradle build tasks, and JUnit running (C-u prefix
;;     enters debug mode, JPDA port 8000).
;;   - dape: DAP client. The built-in `jdtls' config launches the main class;
;;     the `jdtls-jpda' config below attaches to the JPDA port of
;;     JUnit/external Tomcat (forum approach: dape -> java-debug adapter
;;     -> target JVM).
;;   - java-server: LuciusChen's toolkit. Multi-JDK switching, external
;;     Tomcat deploy/stop, Spring Boot run/stop, hot code replace (HCR).
;;
;; External dependencies:
;;   - JDK 17+ (required by jdtls; switch with java-server-select-jdk
;;     for older projects)
;;   - Maven or Gradle (build; prefer the project's own wrapper)
;;   - java-debug plugin jar (required by dape; run `mvn -DskipTests package'
;;     in the microsoft/java-debug repo; the jar lands under extension/server/)
;;   - External Tomcat only needed for WAR deployment (macOS: brew install tomcat@9)

(use-package eglot-java
  :preface
  (defun my/java-debug-plugin-jar ()
    ;; Return the first java-debug plugin jar found in common locations.
    (let ((roots (list (expand-file-name "java-debug" my/cache-dir)
                       (expand-file-name "java-debug" "~")
                       (expand-file-name "debug-adapters" my/cache-dir)
                       "/tmp/java-debug")))
      (catch 'found
        (dolist (root roots)
          (dolist (sub '("extension/server" "server" ""))
            (let ((dir (expand-file-name sub root)))
              (when (file-directory-p dir)
                (dolist (file (directory-files
                               dir t "^com\\.microsoft\\.java\\.debug\\.plugin-.*\\.jar$"))
                  (throw 'found file)))))))))
  (defun my/eglot-java-init-options (_server _jdt)
    ;; JDTLS initialization options: load the java-debug bundle for dape.
    (when-let* ((jar (my/java-debug-plugin-jar)))
      `(:bundles [,jar])))
  :ensure t
  :after eglot
  :hook ((java-mode java-ts-mode) . eglot-java-mode)
  :custom
  ;; eglot-java rewrites `eglot-server-programs' by default; keep manual
  ;; control so java-mode and java-ts-mode share the same jdtls entry.
  (eglot-java-eglot-server-programs-manual-updates t)
  ;; Keep jdtls workspace metadata in the cache dir (the old `-data')
  (eglot-java-eclipse-jdt-cache-directory
   (expand-file-name "jdtls-workspace" my/cache-dir))
  :config
  ;; Load the java-debug bundle so dape can spawn the adapter via JDTLS
  (setq eglot-java-user-init-opts-fn #'my/eglot-java-init-options)
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . eglot-java--eclipse-contact))
  :bind (:map eglot-java-mode-map
         ("C-c j n" . eglot-java-file-new)
         ("C-c j N" . eglot-java-project-new)
         ("C-c j t" . eglot-java-run-test)          ; C-u = debug (JPDA :8000)
         ("C-c j T" . eglot-java-project-build-task)
         ("C-c j R" . eglot-java-project-build-refresh)
         ("C-c j m" . eglot-java-run-main)          ; C-u = debug
         ("C-c j u" . eglot-java-upgrade-lsp-server)
         ("C-c j U" . eglot-java-upgrade-junit-jar)))

(use-package dape
  :preface
  (defun my/dape-jdtls-jpda-ensure (_config)
    ;; Ensure the current buffer has a JDTLS server with java-debug.
    (let ((server (and (featurep 'eglot) (eglot-current-server))))
      (unless server
        (user-error "No active JDTLS (eglot) server in buffer %s" (current-buffer)))
      (unless (seq-contains-p
               (ignore-errors (eglot--server-capable :executeCommandProvider :commands))
               "vscode.java.startDebugSession")
        (user-error "JDTLS does not have the java-debug plugin loaded; cannot start the debug adapter")))
    t)
  (defun my/dape-jdtls-jpda-fn (config)
    ;; Point dape at a java-debug adapter spawned by the current JDTLS.
    ;; The target JVM's JPDA port comes from `:jpda-port' (default 8000,
    ;; matching the debug port used by `eglot-java-run-test').
    (let* ((server (eglot-current-server))
           (adapter-port (eglot-execute-command
                          server "vscode.java.startDebugSession" nil)))
      (thread-first config
        (plist-put 'host "localhost")
        (plist-put 'port adapter-port)
        (plist-put :type "java")
        (plist-put :request "attach")
        (plist-put :hostName "localhost")
        (plist-put :port (or (plist-get config :jpda-port) 8000))
        (plist-put :projectName (project-name (project-current t))))))
  :ensure t
  :custom
  ;; Use `kbd' (not a raw string): dape calls `global-set-key' with this
  ;; value at load time, and a plain "C-c d" string would be read as the
  ;; literal characters C - c SPC d instead of a key sequence.
  (dape-key-prefix (kbd "C-c d"))
  (dape-buffer-window-arrangement 'right)
  :config
  (repeat-mode +1)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  ;; JPDA attach for JUnit / external Tomcat (forum approach):
  ;; dape -> java-debug adapter (spawned by JDTLS) -> target JVM (JPDA port)
  (add-to-list 'dape-configs
               '(jdtls-jpda
                 modes (java-mode java-ts-mode)
                 ensure my/dape-jdtls-jpda-ensure
                 fn my/dape-jdtls-jpda-fn
                 :request "attach"
                 :type "java"
                 :hostName "localhost"
                 :jpda-port 8000
                 :projectName nil)))

;; java-server: LuciusChen's Java server development toolkit
;; (multi-JDK switching, Tomcat deploy, Spring Boot run/stop, HCR)
(use-package java-server
  :ensure (:host github :repo "LuciusChen/java-server")
  :after (eglot dape)
  :hook ((java-mode java-ts-mode) . java-server-mode)
  :custom
  ;; Keep generated artifacts in the cache dir (XDG semantics);
  ;; don't clutter ~/.emacs.d
  (java-server-debug-adapters-dir (expand-file-name "debug-adapters" my/cache-dir))
  (java-server-tomcat-instances-dir (expand-file-name "java-server/tomcat" my/cache-dir))
  (java-server-direct-attach-hcr-helper-dir (expand-file-name "java-server/hcr" my/cache-dir))
  :bind (:map java-server-mode-map
         ("C-c J j" . java-server-select-jdk)
         ("C-c J a" . java-server-auto-select-jdk)
         ("C-c J t" . java-server-tomcat-deploy)    ; C-u = JPDA debug
         ("C-c J T" . java-server-tomcat-stop)
         ("C-c J s" . java-server-spring-boot-run)  ; C-u = JPDA debug
         ("C-c J S" . java-server-spring-boot-stop)
         ("C-c J h" . java-server-hot-replace)
         ("C-c J d" . dape)))

;;; --- Eglot: Angular / web-mode ---
;; ngserver (from @angular/language-server) for Angular projects,
;; typescript-language-server otherwise.  Since Eglot allows one server per
;; buffer, Angular .html buffers use `rass' to merge ngserver +
;; vscode-html-language-server + vscode-css-language-server into one
;; connection; other projects fall back to the default HTML server.
;; Install: npm install -g @angular/language-server @angular/language-service
;;          typescript typescript-language-server vscode-langservers-extracted
;;
;; typescript-ts-mode / tsx-ts-mode / html-ts-mode are built-in (Emacs 29+)
;; and autoloaded, so plain symbol references suffice; no :ensure.
;; typescript-mode is the separate GNU ELPA package; it never matches when
;; absent.
;;
;; Helpers are only used here, so they live in this use-package.  Note: the
;; rules are registered when web-mode first loads; opening .ts before any HTML
;; template falls back to the default typescript-language-server.
(use-package web-mode
  :ensure t
  :config
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
    "Return the HTML server contact for Angular or generic projects.

Angular projects merge ngserver + vscode-html-language-server +
vscode-css-language-server via `rass'; others use the default HTML server."
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
                   . my/angular-web-contact))))

;;; Elisp linting/static checks (Emacs Lisp has no LSP server)
(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . flymake-mode)
         ;; Keep only the checkdoc backend for config files: the byte-compile
         ;; subprocess load-path only contains "./", so it cannot see Elpaca
         ;; packages and produces a lot of "function not defined" noise.
         (emacs-lisp-mode . (lambda ()
                              (remove-hook 'flymake-diagnostic-functions
                                           #'elisp-flymake-byte-compile t))))
  :config
  ;; Emacs 30's `emacs-lisp-mode' registers two flymake backends by default:
  ;; `elisp-flymake-byte-compile' (compile errors; removed above) and
  ;; `elisp-flymake-checkdoc' (doc/style; kept).
  (setq-default checkdoc-package-keywords-flag nil))

;;; --- Markdown: code block editing ---
;; markdown-mode 2.8+ (MELPA).  Code blocks get the language's major mode:
;; native font-lock in place, and a dedicated indirect buffer for editing via
;; `C-c '' (`markdown-edit-code-block', needs `edit-indirect').  The mode is
;; picked by `markdown-get-lang-mode': explicit `markdown-code-lang-modes'
;; first, then *-ts-mode when the tree-sitter grammar is available, else
;; plain *-mode.
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  ;; "ts"/"js" grammars are named "typescript"/"javascript", so the ts-modes
  ;; can't be inferred from the fence language.
  (dolist (pair '(("ts" . typescript-ts-mode)
                  ("js" . js-ts-mode)))
    (add-to-list 'markdown-code-lang-modes pair)))

(use-package edit-indirect
  :ensure t
  :after markdown-mode)

;; LLM coding agents in Emacs. Independent, pick one per task.
;; Requires `codex` and `pi` CLIs on PATH.

;; Codex: native client for `codex app-server`. Open via M-x codex-ide-menu.
(use-package codex-ide
  :ensure (:host github :repo "dgillis/emacs-codex-ide")
  :bind (("C-c C-a" . codex-ide-menu))
  :config
  ;; No line numbers in IDE panels/session buffers
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
