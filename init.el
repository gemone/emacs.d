;;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;;; 00 - Performance Optimization
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

;;; 01 - Package Manager (Elpaca)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
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
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when (memq system-type '(ms-dos windows-nt))
  (elpaca-no-symlink-mode))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; 02 - Helper Functions
;; Terminal functions
(defun gemo/toggle-term ()
  "Toggle terminal popup."
  (interactive)
  (let ((buf (get-buffer "*terminal*")))
    (if (and buf (get-buffer-window buf))
        (delete-window (get-buffer-window buf))
      (split-window-sensibly (selected-window) nil t)
      (other-window 1)
      (if buf
          (switch-to-buffer buf)
        (ansi-term "zsh")))))

(defun gemo/new-terminal ()
  "Create a new terminal."
  (interactive)
  (split-window-sensibly (selected-window) nil t)
  (other-window 1)
  (ansi-term "zsh"))

;; Eglot helper functions
(defun gemo/eglot-should-manage-p ()
  "Check if current buffer should be managed by Eglot."
  (and (buffer-file-name)                           ; Must be a file buffer
       (not (string-prefix-p " " (buffer-name)))    ; Not a temporary buffer
       (not (string-match-p "markdown-code-fontification" (buffer-name))) ; Not Markdown code block
       (not (string-match-p "\\` \\*" (buffer-name))))) ; Not internal buffer

(defun gemo/eglot-ensure-maybe ()
  "Conditionally start Eglot based on buffer properties.
This function is used as a hook to avoid starting Eglot in temporary buffers."
  (unless (or (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode 'ron-mode)
              (not (gemo/eglot-should-manage-p)))
    (when (eglot--guess-contact)
      (eglot-ensure))))

(defun gemo/eglot-ensure-if-appropriate (orig-fun &rest args)
  "Only start Eglot in appropriate buffers, otherwise skip.
Used as :around advice for eglot-ensure."
  (when (gemo/eglot-should-manage-p)
    (apply orig-fun args)))

;; Zoxide helper
(defun gemo/zoxide-open-with-dired ()
  "Open zoxide directory in dired."
  (interactive)
  (if current-prefix-arg
      (zoxide-open-with nil (lambda (file) (dired-other-window file)) t)
    (zoxide-open-with nil (lambda (file) (dired file)) t)))

;;; 03 - UI Settings
(use-package emacs
  :ensure nil
  :init
  (add-hook 'window-setup-hook #'toggle-frame-maximized)
  :config
  (global-display-line-numbers-mode t)
  :custom
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  :init (doom-modeline-mode 1))

(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-scale-factor 1.0)
  (when (display-graphic-p)
    (setq nerd-icons-font-family "CaskaydiaCove Nerd Font")))

;;; 04 - Font Configuration
(use-package cnfonts
  :ensure t
  :demand t
  :custom
  (cnfonts-use-system-type t)
  (cnfonts-personal-fontnames
   '(("CaskaydiaCove Nerd Font" "Fira Code" "JetBrains Mono"
      "SF Mono" "Menlo" "Monaco" "Consolas" "Cascadia Code"
      "DejaVu Sans Mono" "Ubuntu Mono" "Liberation Mono"
      "Hack" "Source Code Pro" "Inconsolata")
     ("LXGW WenKai Mono"
      "PingFang SC" "PingFang TC" "Hiragino Sans GB"
      "Microsoft YaHei" "SimHei" "SimSun"
      "Noto Sans Mono CJK SC" "Noto Sans CJK SC"
      "WenQuanYi Zen Hei" "WenQuanYi Micro Hei"
      "Source Han Sans CN")
     ("HanaMinB")
     ("Segoe UI Symbol" "Symbola" "Apple Symbols" "Arial Unicode MS")
     ("NanumGothic" "Arial Unicode MS")))
  :config
  (cnfonts-mode 1)
  (with-eval-after-load 'evil
    (add-to-list 'evil-emacs-state-modes 'cnfonts-ui-mode))
  :bind
  (:map cnfonts-mode-map
        ("C--" . cnfonts-decrease-fontsize)
        ("C-=" . cnfonts-increase-fontsize)))

(use-package unicad
  :ensure t
  :demand t
  :config (unicad-mode))

;;; 05 - Keybindings
(use-package evil
  :ensure t
  :demand t
  :preface
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump))

(use-package evil-collection
  :after evil
  :ensure t
  :demand t
  :config (evil-collection-init))

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-override-mode)
  (general-create-definer gemo/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-define-key
   :states 'normal
   :keymaps 'override
   "\\" '(nil :which-key "config"))

  (general-define-key
   :states 'normal
   :prefix "\\"
   :non-normal-prefix "C-\\"
   "cc" '(lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory)) :which-key "config init.el")
   "cd" '(lambda () (interactive) (dired user-emacs-directory) :which-key "config directory"))

  (gemo/leader-keys
    "SPC" '(execute-extended-command :which-key "M-x")
    "!"   '(shell-command :which-key "Shell command")

    ;; Buffer Management
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(switch-to-buffer :which-key "Switch buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Prev buffer")
    "br" '(revert-buffer :which-key "Revert buffer")

    ;; Open
    "o"  '(:ignore t :which-key "open")
    "ot" '(gemo/toggle-term :which-key "Toggle terminal")
    "oT" '(gemo/new-terminal :which-key "New terminal")))

(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-secondary-delay 0.1)
  :config (which-key-mode))

;;; 06 - Completion Framework
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  :config (corfu-popupinfo-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; 07 - Development Tools
(use-package editorconfig
  :ensure t
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist
		'(("lambda" . ?λ)
		  ("<-"     . ?←)
		  ("->"     . ?→)
		  ("->>"    . ?↠)
		  ("=>"     . ?⇒)
		  ("map"    . ?↦)
		  ("/="     . ?≠)
		  ("!="     . ?≠)
		  ("=="     . ?≡)
		  ("<="     . ?≤)
		  (">="     . ?≥)
		  ("=<<"    . (?= (Br . Bl) ?≪))
		  (">>="    . (?≫ (Br . Bl) ?=))
		  ("<=<"    . ?↢)
		  (">=>"    . ?↣)
		  ("&&"     . ?∧)
		  ("||"     . ?∨)
		  ("not"    . ?¬)))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package eglot
  :ensure nil
  :demand t
  :hook (prog-mode . gemo/eglot-ensure-maybe)
  :init
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5
        eglot-sync-timeout 30
        eglot-connect-timeout 30
        eglot-ignored-server-capabilities '(:documentFormattingProvider)
        eglot-report-progress nil)
  :general
  (gemo/leader-keys
    "c"  '(:ignore t :which-key "code/lsp")
    "ca" '(eglot-code-actions :which-key "Code actions")
    "cr" '(eglot-rename :which-key "Rename symbol")
    "cf" '(eglot-format :which-key "Format buffer")
    "cd" '(xref-find-definitions :which-key "Go to definition")
    "cD" '(xref-find-references :which-key "Find references"))
  :config
  (advice-add 'eglot-ensure :around #'gemo/eglot-ensure-if-appropriate))

(use-package reformatter
  :ensure t)

(use-package zig-mode
  :ensure t
  :mode (("\\.zig\\'" . zig-mode)
         ("\\.zon\\'" . zig-mode))
  :custom
  (zig-compiler "zig")
  (zig-build-dir "zig-cache")
  :general
  (gemo/leader-keys
    :keymaps 'zig-mode-map
    "m"  '(:ignore t :which-key "zig")
    "mc" '(zig-compile :which-key "Compile")
    "mr" '(zig-run :which-key "Run")
    "mt" '(zig-test :which-key "Run tests")
    "mf" '(zig-format-buffer :which-key "Format buffer"))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

;;; 08 - File and Tools
(use-package dired
  :ensure nil
  :config
  (setq-default dired-dwim-target t)
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil))
  (require 'dired-x)
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :general
  (gemo/leader-keys
    "d"  '(:ignore t :which-key "dired")
    "dd" '(dired-jump :which-key "Jump to dired")
    "dj" '(dired-jump :which-key "Jump to file in dired"))
  :bind
  (:map dired-mode-map
        ("C-c C-j" . dired-jump)
        ("a" . dired-find-alternate-file)
        ("i" . dired-subdir-insert)
        ("C-k" . dired-do-kill-lines)
        ("*" . nil)))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode)
  :config (diredfl-global-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  (setq nerd-icons-dired-monochrome nil))

(use-package zoxide
  :ensure (:type git :host sourcehut :repo "vonfry/zoxide.el")
  :hook (dired-mode . (lambda ()
                        (local-set-key (kbd "P") 'gemo/zoxide-open-with-dired)))
  :general
  (gemo/leader-keys
    "j"  '(:ignore t :which-key "jump")
    "jz" '(zoxide-travel :which-key "Zoxide travel")))

(use-package consult
  :ensure t
  :general
  (gemo/leader-keys
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "Search lines")
    "si" '(consult-imenu :which-key "Jump to symbol")
    "so" '(consult-outline :which-key "Search outline/symbols")))

(use-package transient
  :ensure t
  :demand t)

(use-package magit
  :ensure t
  :after transient
  :general
  (gemo/leader-keys
    "g"  '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "Magit status")))

(use-package rg
  :ensure t
  :commands (rg rg-menu rg-literal)
  :init
  (with-eval-after-load 'rg
    (rg-enable-default-bindings))
  :config
  (setq rg-group-result t
        rg-hide-command t))

;;; init.el ends here
