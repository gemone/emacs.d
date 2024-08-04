;; Automatically tangle our Emacs.org config file when we save it
(defun emaconf/org-babel-tangle ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'emaconf/org-babel-tangle)))

(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
(defconst *is-win* (string-equal system-type "windows-nt"))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun emaconf/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'emaconf/display-startup-time)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package el-patch)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)			; Disable visible scroolbar
(tool-bar-mode -1)			; Disable the toolbar
(tooltip-mode -1)			; Disable tooltips
(set-fringe-mode -1)			; Give some breathing room

(menu-bar-mode -1)			; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package fontaine
  :ensure t
  :when (display-graphic-p)
  ;; :hook (kill-emacs . fontaine-store-latest-preset)
  :config
  (setq fontaine-presets
	'((regular
           :default-height 140
           :default-weight regular
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.0
           )
          (large
           :default-height 180
           :default-weight normal
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.05
           )
          (t
           :default-family "CaskaydiaCove Nerd Font Mono"
           :fixed-pitch-family "CaskaydiaCove Nerd Font Mono"
           :variable-pitch-family "CaskaydiaCove Nerd Font Mono"
           :italic-family "CaskaydiaCove Nerd Font Mono Italic"
           :blod-family "CaskaydiaCove Nerd Font Mono Bold"
           :variable-pitch-weight normal
           :bold-weight bold
           :italic-slant italic
           :line-spacing 0.1)
          ))
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-set-preset 'regular)

  ;; set emoji font
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji)
   (cond
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ))

  ;; set Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family
		(cond
		 ((eq system-type 'darwin)
		  (cond
		   ((member "PingFang SC" (font-family-list)) "PingFang SC")
		   ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
		   ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
		   ))
		 ((eq system-type 'gnu/linux)
		  (cond
		   ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
		   ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
		   ))
		 (t
		  (cond
		   ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
		   ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
		   )))
		)))

  ;; set Chinese font scale
  (setq face-font-rescale-alist `(
				  ("Symbola"             . 1.3)
				  ("Microsoft YaHei"     . 1.2)
				  ("WenQuanYi Zen Hei"   . 1.2)
				  ("Sarasa Term SC Nerd" . 1.2)
				  ("PingFang SC"         . 1.16)
				  ("Lantinghei SC"       . 1.16)
				  ("Kaiti SC"            . 1.16)
				  ("Yuanti SC"           . 1.16)
				  ("Apple Color Emoji"   . 0.91)
				  ))
  )

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)

(load-theme 'modus-vivendi t)
;; (load-theme 'modus-operandi t)

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 12)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer emaconf/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (emaconf/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tc" '(counsel-load-theme :which-key "choose theme")
    "tt" '(modus-themes-toggle :which-key "change modus theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.config/emacs/README.org")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy counsel nerd-icons-ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package nerd-icons-ivy-rich
  :init
  (nerd-icons-ivy-rich-mode 1))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq delete-by-moving-to-trash t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  (add-to-list 'dired-open-functions #'dired-open-xdg t)
  ;; (setq dired-open-extensions '(("png" . "feh")
  ;; ("mkv" . "mpv")))
  )

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
	  treemacs-file-extension-regex            treemacs-last-period-regex-value
	  treemacs-file-follow-delay               0.2
	  treemacs-file-name-transformer           #'identity
	  treemacs-follow-after-init               t
	  treemacs-expand-after-init               t
	  treemacs-find-workspace-method           'find-for-file-or-pick-first
	  treemacs-git-command-pipe                ""
	  treemacs-goto-tag-strategy               'refetch-index
	  treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	  treemacs-hide-dot-git-directory          t
	  treemacs-indentation                     2
	  treemacs-indentation-string              " "
	  treemacs-is-never-other-window           nil
	  treemacs-max-git-entries                 5000
	  treemacs-missing-project-action          'ask
	  treemacs-move-files-by-mouse-dragging    t
	  treemacs-move-forward-on-expand          nil
	  treemacs-no-png-images                   nil
	  treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          nil
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                        'left
	  treemacs-read-string-input               'from-child-frame
	  treemacs-recenter-distance               0.1
	  treemacs-recenter-after-file-follow      nil
	  treemacs-recenter-after-tag-follow       nil
	  treemacs-recenter-after-project-jump     'always
	  treemacs-recenter-after-project-expand   'on-distance
	  treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	  treemacs-project-follow-into-home        nil
	  treemacs-show-cursor                     nil
	  treemacs-show-hidden-files               t
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-space-between-root-nodes        t
	  treemacs-tag-follow-cleanup              t
	  treemacs-tag-follow-delay                1.5
	  treemacs-text-scale                      nil
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           35
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(treemacs-start-on-boot)

(defun emaconf/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMonoNL Nerd Font" :weight 'regular :height (cdr face)))
  )

(defun emaconf/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . emaconf/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (emaconf/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun emaconf/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . emaconf/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package evil-nerd-commenter
  :diminish
  :bind (("M-/" . evilnc-comment-or-uncomment-lines)
         :map evil-normal-state-map
         ("gcc" . evilnc-comment-or-uncomment-lines)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun emaconf/configure-shell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-commond-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-define-key '(insert) eshell-mode-map (kbd "C-a") 'eshell-bol)
  (evil-define-key '(insert) eshell-mode-map (kbd "C-e") 'eshell-show-maximum-output)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input nil))

(use-package eshell-git-prompt)
(use-package eshell
  :straight nil
  :hook (eshell-first-time-mode . emaconf/configure-shell)
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (yas-minor-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

"python3 -m venv ./venv"

"pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz"

(defvar python-path (expand-file-name "./venv/bin/python"))
(setq python-interpreter python-path)
(setq python-shell-interpreter python-path)

(defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
  (let* ((json-object-type 'plist)
         (custom-dir (expand-file-name ".cache/lsp-bridge/pyright" user-emacs-directory))
         (custom-config (expand-file-name "pyright.json" custom-dir))
         (default-config (json-read-file (expand-file-name "repo/lsp-bridge/langserver/pyright.json" user-emacs-directory)))
         (settings (plist-get default-config :settings))
         )

    (plist-put settings :pythonPath (executable-find "python"))

    (make-directory (file-name-directory custom-config) t)

    (with-temp-file custom-config
      (insert (json-encode default-config)))

    custom-config))

(use-package lsp-bridge
  :after yasnippet markdown-mode
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
  			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  			 :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :hook
  (python-mode-hook . (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project))
  (pyvenv-post-activate-hooks . lsp-bridge-restart-process))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))
