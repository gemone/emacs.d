;;; init.el

;;; 00 - Performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000) ; Set to ~2MB
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

;;; 01 - Package Manager
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

;;; 02 - UI Settings
(use-package emacs
  :ensure nil
  :init
  ;; Maximize frame on startup
  (add-hook 'window-setup-hook #'toggle-frame-maximized)

  :config
  (global-display-line-numbers-mode t)
  :custom
  ;;  --- FOR vertico
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  )

(use-package nerd-icons
  :ensure t
  :config
  (when (display-graphic-p)
    (setq nerd-icons-font-family "CaskaydiaCove Nerd Font")
    )
  (setq nerd-icons-scale-factor 1.0)
  )

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
  ;; Disable icons to avoid font issues
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  :init (doom-modeline-mode 1))

;;; 04 - Keybindings
(use-package evil
  :ensure t
  :demand t
  :preface
  (setq evil-want-integration t)   ; Enable evil integration
  (setq evil-want-keybinding nil)   ; We'll use evil-collection
  :config
  (evil-mode 1)
  ;; Bind '-' to dired-jump in normal state
  (define-key evil-normal-state-map (kbd "-") 'dired-jump))

(use-package evil-collection
  :after evil
  :ensure t
  :demand t
  :config
  (evil-collection-init))

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

  ;; Backslash keybindings for config
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
    "oT" '(gemo/new-terminal :which-key "New terminal")
    ))

(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-secondary-delay 0.1)
  :config
  (which-key-mode))

;;; 03 - Font
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
  ;; Use Emacs state in cnfonts-ui-mode to avoid evil keybinding conflicts
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

;;; 05 - Completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

(use-package savehist
  :ensure nil ; Built-in
  :init
  (savehist-mode 1))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

;;; 06 - Tools
(use-package consult
  :ensure t
  :general
  (gemo/leader-keys
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "Search lines")
    "si" '(consult-imenu :which-key "Jump to symbol")
    "so" '(consult-outline :which-key "Search outline/symbols") ; Quick jump in file
    ))

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

;; Zoxide - Smart directory jump
(use-package zoxide
  :ensure (:type git :host sourcehut :repo "vonfry/zoxide.el")
  :hook (dired-mode . (lambda ()
                        (local-set-key (kbd "P") 'zoxide-open-with-dired)))
  :config
  (defun zoxide-open-with-dired ()
    "Open zoxide directory in dired."
    (interactive)
    (if current-prefix-arg
        (zoxide-open-with nil (lambda (file) (dired-other-window file)) t)
      (zoxide-open-with nil (lambda (file) (dired file)) t)))
  :general
  (gemo/leader-keys
    "j"  '(:ignore t :which-key "jump")
    "jz" '(zoxide-travel :which-key "Zoxide travel")))

;; Dired enhancements (Purcell style)
(use-package dired
  :ensure nil
  :config
  ;; DWIM target
  (setq-default dired-dwim-target t)
  ;; Load dired-x
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
        ("a" . dired-find-alternate-file)  ; reuse buffer
        ("i" . dired-subdir-insert)        ; insert subdir
        ("C-k" . dired-do-kill-lines)     ; hide lines
        ("*" . nil)))                      ; disable * prefix

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode)
  :config (diredfl-global-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  (setq nerd-icons-dired-monochrome nil))  ; Default size for better display

;;; 07 - Code (Coding-Mode)

(use-package treesit
  :ensure nil
  :preface
  ;; Configure tree-sitter language sources before package loads
  (setq treesit-language-source-alist
	'((bash       "https://github.com/tree-sitter/tree-sitter-bash" "master")
          (c          "https://github.com/tree-sitter/tree-sitter-c" "master")
          (cmake      "https://github.com/uyha/tree-sitter-cmake" "master")
          (cpp        "https://github.com/tree-sitter/tree-sitter-cpp" "master")
          (css        "https://github.com/tree-sitter/tree-sitter-css" "master")
          (go         "https://github.com/tree-sitter/tree-sitter-go" "master")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod" "main")
          (html       "https://github.com/tree-sitter/tree-sitter-html" "master")
          (java       "https://github.com/tree-sitter/tree-sitter-java" "master")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master")
          (json       "https://github.com/tree-sitter/tree-sitter-json" "master")
          (make       "https://github.com/alemuller/tree-sitter-make" "master")
          (python     "https://github.com/tree-sitter/tree-sitter-python" "master")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml" "master")
          (zig        "https://github.com/tree-sitter-grammars/tree-sitter-zig" "master")))

  ;; Helper functions for tree-sitter grammar management
  ;;;###autoload
  (defun gemo/treesit-supported-p ()
    "Check if current Emacs supports tree-sitter."
    (interactive)
    (if (treesit-available-p)
	(message "✓ Tree-sitter is supported (Emacs %d.%d)"
		 emacs-major-version emacs-minor-version)
      (message "✗ Tree-sitter not available")))

  ;;;###autoload
  (defun gemo/treesit-check-grammars ()
    "Check which tree-sitter grammars are installed."
    (interactive)
    (when (gemo/treesit-supported-p)
      (let ((installed 0) (missing 0))
	(dolist (lang (mapcar 'car treesit-language-source-alist))
          (if (treesit-ready-p lang t)
              (progn (message "✓ %s" lang) (cl-incf installed))
            (progn (message "✗ %s" lang) (cl-incf missing))))
	(message "\nSummary: %d installed, %d missing" installed missing))))

  ;;;###autoload
  (defun gemo/treesit-install-grammar (lang)
    "Install a single tree-sitter grammar for LANG."
    (interactive
     (list (completing-read "Install language grammar: "
			    (mapcar #'symbol-name (mapcar 'car treesit-language-source-alist)))))
    (when (gemo/treesit-supported-p)
      (let ((lang-symbol (intern lang)))
	(if (treesit-ready-p lang-symbol t)
            (message "✓ %s grammar is already installed!" lang)
          (message "Installing %s grammar..." lang)
          (condition-case err
              (progn
		(treesit-install-language-grammar lang-symbol)
		(message "✓ Successfully installed %s grammar!" lang))
            (error
             (message "✗ Failed to install %s grammar: %S" lang (cdr err))))))))

  ;;;###autoload
  (defun gemo/treesit-install-missing-grammars ()
    "Install all missing tree-sitter grammars."
    (interactive)
    (when (gemo/treesit-supported-p)
      (let ((missing 0) (failed 0))
	(dolist (lang (mapcar 'car treesit-language-source-alist))
          (unless (treesit-ready-p lang t)
            (cl-incf missing)
            (message "Installing %s grammar..." lang)
            (condition-case err
		(treesit-install-language-grammar lang)
              (error
               (message "✗ Failed to install %s: %S" lang (cdr err))
               (cl-incf failed)))))
	(if (> missing 0)
            (message "Installation complete: %d succeeded, %d failed."
                     (- missing failed) failed)
          (message "All grammars are already installed!")))))

  ;; Add user's tree-sitter directory to search path
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "tree-sitter" user-emacs-directory))

  :init
  ;; Major mode remapping for tree-sitter modes
  (dolist (mapping
	   '((bash-mode        . bash-ts-mode)
             (c-mode           . c-ts-mode)
             (c++-mode         . c++-ts-mode)
             (cmake-mode       . cmake-ts-mode)
             (css-mode         . css-ts-mode)
             (go-mode          . go-ts-mode)
             (go-mod-mode      . go-mod-ts-mode)
             (html-mode        . html-ts-mode)
             (java-mode        . java-ts-mode)
             (js-json-mode     . json-ts-mode)
             (js-mode          . js-ts-mode)
             (js2-mode         . js-ts-mode)
             (makefile-mode    . makefile-ts-mode)
             (python-mode      . python-ts-mode)
             (typescript-mode  . typescript-ts-mode)
             (yaml-mode        . yaml-ts-mode)
             (zig-mode         . zig-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  ;; File type associations
  (dolist (mapping
	   '(("CMakeLists\\.txt\\'" . cmake-ts-mode)
             ("\\.cmake\\'" . cmake-ts-mode)
             ("\\.cmake\\.in\\'" . cmake-ts-mode)
             ("/go\\.mod\\'" . go-mod-ts-mode)
             ("/go\\.sum\\'" . go-mod-ts-mode)
             ("\\.json\\'" . json-ts-mode)
             ("\\.tsx\\'" . tsx-ts-mode)
             ("\\.zig\\'" . zig-ts-mode)
             ("\\.zig\\.zon\\'" . zig-ts-mode)))
    (add-to-list 'auto-mode-alist mapping)))

;; Load zig-ts-mode if available (requires Emacs 31+ or manual installation)
(use-package zig-ts-mode
  :load-path "lisp/progmodes"
  :if (treesit-ready-p 'zig t))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))
