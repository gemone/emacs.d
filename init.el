;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;;; 00.1 - Load Options
(let ((options (expand-file-name "options.el" user-emacs-directory)))
  (when (file-exists-p options)
    (load options)))

;; Load local configuration overrides
(let ((options-local (expand-file-name "options.local.el" user-emacs-directory)))
  (when (file-exists-p options-local)
    (load options-local)))

;; Configure custom file for Customize interface settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load early-init.local.el if it exists (for additional early init)
(let ((early-init-local (expand-file-name "early-init.local.el" user-emacs-directory)))
  (when (file-exists-p early-init-local)
    (load early-init-local)))

;;; 00 - Performance Optimization
(defvar default-file-name-handler-alist file-name-handler-alist)
;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold (* 256 1024 1024)  ; 256MB during startup
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Lower GC threshold after startup for normal use
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB during normal use
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)

            ;; Windows-specific optimizations
            (when (eq system-type 'windows-nt)
              ;; Speed up font rendering
              (setq inhibit-compacting-font-cache t)
              ;; Speed up projectile (if installed)
              (setq projectile-git-submodule-command nil))))

;;; 01 - Package Manager (package.el)

;; Initialize package sources
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package.el
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(require 'use-package-ensure)
(require 'use-package-core)  ; Required for general to register :general keyword
(setq use-package-always-ensure t)  ; Auto-install packages
(setq use-package-always-defer t)   ; Defer loading by default

;; Ensure general is installed and loaded BEFORE other use-package forms
;; This registers the :general keyword with use-package
(unless (package-installed-p 'general)
  (package-refresh-contents)
  (package-install 'general))
(require 'general)

;; Verify general registered with use-package
(message "General loaded, use-package keywords registered")
(general-auto-unbind-keys)
(general-override-mode)

;; General keybindings leader definer
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
  "br" '(revert-buffer :which-key "Revert buffer"))

(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-secondary-delay 0.1)
  :config (which-key-mode))

;;; 02 - Helper Functions

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
  :hook (window-setup . toggle-frame-maximized)
  :config
  (global-display-line-numbers-mode t)

  ;; Disable scrollbars for current frame
  (set-frame-parameter nil 'scroll-bar-width nil)
  (set-frame-parameter nil 'scroll-bar-height nil)
  (set-frame-parameter nil 'vertical-scroll-bars nil)
  (set-frame-parameter nil 'horizontal-scroll-bars nil)

  ;; Apply settings to new frames
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (set-frame-parameter frame 'scroll-bar-width nil)
                (set-frame-parameter frame 'scroll-bar-height nil)
                (set-frame-parameter frame 'vertical-scroll-bars nil)
                (set-frame-parameter frame 'horizontal-scroll-bars nil))))

  ;; Fringe and window divider settings
  (setf (frame-parameter nil 'right-divider-width) 0)
  (setf (frame-parameter nil 'bottom-divider-width) 0)
  (setq fringe-mode '(8 . 8))  ; 设置左右 fringe 宽度

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
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-major-mode-color-icon nil)
  :init (doom-modeline-mode 1))

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
  :init
  (setq evil-undo-system 'undo-fu)  ; Must set before evil loads
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

;; undo-fu for better undo/redo with evil
(use-package undo-fu
  :ensure t)

;; vundo for visual undo tree (modern alternative to undo-tree)
(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-window-max-height 8)
  :bind
  (("C-x u" . vundo)))

;; undo-fu-session for persistent undo history
(use-package undo-fu-session
  :ensure t
  :demand t
  :after undo-fu
  :config
  (undo-fu-session-global-mode 1)
  (setq undo-fu-session-directory
        (expand-file-name ".undo-fu-session" user-emacs-directory))
  (setq undo-fu-session-compression 'gzip))

;; Evil Nerd Commenter for gcc-style commenting
(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :config
  (evilnc-default-hotkeys))

;; Vimish-fold for Vim-like folding
(use-package vimish-fold
  :ensure t
  :hook ((prog-mode . vimish-fold-mode)
         (prog-mode . hs-minor-mode))
  :config
  (setq vimish-fold-indication-mode 'right-fringe))

;; evil-vimish-fold integration
(use-package evil-vimish-fold
  :ensure t
  :after (evil vimish-fold)
  :config
  (global-evil-vimish-fold-mode 1))

;;; 06 - Completion Framework (vertico for minibuffer)
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

;; Note: corfu is disabled because lsp-bridge uses acm (Asynchronous Completion Menu)
;; If you need corfu for non-lsp modes, you can enable it conditionally

;;; 07 - Development Tools

;; ============================================
;; LSP-Bridge Configuration
;; ============================================

;; Add lsp-bridge to load-path (git submodule)
(add-to-list 'load-path (expand-file-name "lsp-bridge" user-emacs-directory))

;; Yasnippet (required by lsp-bridge)
(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode 1))

;; LSP-Bridge - Fast LSP client using multi-threading
(use-package lsp-bridge
  :ensure nil
  :demand t
  :hook (doom-load-theme . (lambda () (acm-frame-init-colors t)))
  :init
  ;; Use .venv Python for lsp-bridge
  (setq lsp-bridge-python-command (expand-file-name ".venv/bin/python3" user-emacs-directory))

  ;; Project root selection - like lsp-mode's behavior
  (defvar gemo/lsp-bridge-project-roots-cache (make-hash-table :test 'equal))

  (defun gemo/lsp-bridge-select-project-root (filepath)
    "Select project root for FILEPATH with popup selection like lsp-mode.
Returns the selected project root directory."
    (let* ((file-dir (file-name-directory (directory-file-name (file-truename filepath))))
           (cached-root (gethash file-dir gemo/lsp-bridge-project-roots-cache)))
      (or cached-root
          (let* ((project-roots (gemo/lsp-bridge-find-project-roots filepath))
                 (selected-root
                  (cond
                   ;; Single root found - use it
                   ((= (length project-roots) 1)
                    (car project-roots))
                   ;; Multiple roots - let user choose
                   ((> (length project-roots) 1)
                    (completing-read "Select project root: " project-roots nil t))
                   ;; No roots found - use file directory
                   (t file-dir))))
            (puthash file-dir selected-root gemo/lsp-bridge-project-roots-cache)
            selected-root))))

  (defun gemo/lsp-bridge-find-project-roots (filepath)
    "Find all potential project roots for FILEPATH by searching upward for project markers."
    (let* ((file-dir (file-name-directory (directory-file-name (file-truename filepath))))
           (project-markers '(".git" ".dir-locals.el"
                              "package.json" "Cargo.toml" "pyproject.toml"
                              "go.mod" "pom.xml" "build.gradle" "angular.json"
                              "compile_commands.json" ".ccls-root"))
           roots)
      ;; Search upward for project markers
      (let ((dir file-dir))
        (while (and dir (not (string-equal dir "/")))
          (dolist (marker project-markers)
            (let ((marker-path (expand-file-name marker dir)))
              (when (or (file-exists-p marker-path)
                        (file-directory-p marker-path))
                (cl-pushnew dir roots :test #'string-equal))))
          (setq dir (file-name-directory (directory-file-name dir)))))
      ;; Also add projectile/project.el roots if available
      (when (fboundp 'projectile-project-p)
        (let ((proj-root (projectile-project-p file-dir)))
          (when proj-root
            (cl-pushnew proj-root roots :test #'string-equal))))
      (when (fboundp 'project-current)
        (let ((proj (project-current nil file-dir)))
          (when proj
            (let ((proj-root (nth 2 proj)))
              (when proj-root
                (cl-pushnew proj-root roots :test #'string-equal))))))
      ;; Return unique roots, preferring deeper ones
      (delete-dups (sort roots #'> :key #'length))))

  ;; Set custom project path function
  (setq lsp-bridge-get-project-path-by-filepath #'gemo/lsp-bridge-select-project-root)

  ;; Enable diagnostics
  (setq lsp-bridge-enable-diagnostics t
        lsp-bridge-enable-signature-help t
        lsp-bridge-enable-search-words t
        lsp-bridge-enable-auto-format-code nil
        lsp-bridge-enable-inlay-hint nil
        lsp-bridge-enable-hover-diagnostic nil
        lsp-bridge-enable-completion-in-string nil
        lsp-bridge-enable-document-highlight nil
        lsp-bridge-completion-obey-trigger-characters-p t)

  ;; Disable backup (lsp-bridge recommendation)
  (setq lsp-bridge-disable-backup t)

  ;; Signature help display
  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        lsp-bridge-signature-show-with-frame-position "bottom-right")

  ;; ACM (Asynchronous Completion Menu) settings
  (setq acm-enable-icon t
        acm-enable-doc t
        acm-enable-doc-markdown-render 'async
        acm-menu-length 10
        acm-doc-frame-max-lines 20)

  ;; Language Server Configuration
  (setq lsp-bridge-c-lsp-server "ccls"
        lsp-bridge-python-lsp-server "pyright")

  :general
  (general-def
    :states 'normal
    :keymaps 'prog-mode-map
    "gd" #'lsp-bridge-find-def
    "gD" #'lsp-bridge-find-type-def
    "gr" #'lsp-bridge-find-references
    "gR" #'lsp-bridge-rename
    "gI" #'lsp-bridge-find-impl
    "K"  #'lsp-bridge-popup-documentation)

  :config
  ;; Start global lsp-bridge mode
  (global-lsp-bridge-mode))

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

;; Common Lisp Development (SLIME)
(use-package slime
  :ensure t
  :hook
  (slime-description-mode . (lambda ()
                              (setq-local font-lock-defaults '(lisp-font-lock-keywords))
                              (font-lock-mode 1)))
  :config
  (setq inferior-lisp-program "sbcl"
        slime-autodoc-interval 0.5
        slime-highlight-edges t)
  (slime-setup '(slime-fancy slime-autodoc))

  ;; Enable acm capf backend for slime completion
  (with-eval-after-load 'acm-backend-capf
    (setq acm-enable-capf t)
    (add-to-list 'acm-backend-capf-mode-list 'slime-mode)
    (add-to-list 'acm-backend-capf-mode-list 'slime-repl-mode)))

;; Tree-sitter automatic configuration
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)           ; Ask before installing grammar
  :config
  (treesit-auto-add-to-auto-mode-alist 'all) ; Add all ts-modes to auto-mode-alist
  (global-treesit-auto-mode))

;; Smartparens configuration
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "('" nil :actions :rem)
  (sp-pair "`" nil :actions nil))

;; Rainbow delimiters for colorful parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================
;; Project Management (Projectile + project.el)
;; ============================================

;; Projectile for project management
(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'default)  ; Use vertico via completion-at-point
  (projectile-sort-order 'recentf)  ; Sort by recently active
  (projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory))
  (projectile-known-projects-file (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))
  (projectile-project-search-path '("~/projects" "~/work" "~"))  ; Project search paths
  (projectile-globally-ignored-files '(".DS_Store" "*.elc" "node_modules" "target" "zig-cache" "zig-out"))
  (projectile-globally-ignored-directories '(".git" "node_modules" "target" "zig-cache" "zig-out" "vendor" ".venv" "venv" "__pycache__"))
  (projectile-indexing-method 'alien)  ; Faster indexing using external tools
  :hook (project-find-functions . project-try-projectile)
  :config
  (projectile-mode +1)
  ;; Integrate with project.el
  (with-eval-after-load 'project
    (defun project-find-projectile (dir)
      "Find Projectile project in DIR."
      (let ((root (projectile-project-p dir)))
        (when root
          (cons 'transient root))))
    (defun project-try-projectile (dir)
      "Try to find Projectile project in DIR."
      (let ((root (projectile-project-p dir)))
        (when root
          (cons 'transient root))))))

;; Project.el configuration
(use-package project
  :ensure nil
  :custom
  (project-list-file (expand-file-name ".project-list.eld" user-emacs-directory))
  ;; Configure project root markers for various project types
  (project-vc-extra-root-markers
   '("package.json"      ; Node.js/JavaScript
     "package-lock.json"
     "yarn.lock"
     "pnpm-lock.yaml"
     "tsconfig.json"     ; TypeScript
     "go.mod"            ; Go
     "go.sum"
     "Cargo.toml"        ; Rust
     "Cargo.lock"
     "pyproject.toml"    ; Python
     "setup.py"
     "requirements.txt"
     "setup.cfg"
     "tox.ini"
     "pom.xml"           ; Java/Maven
     "build.gradle"      ; Java/Gradle
     "build.gradle.kts"
     "settings.gradle"
     "settings.gradle.kts"
     "build.sbt"         ; Scala
     "project.clj"       ; Clojure
     "deps.edn"
     "Gemfile"           ; Ruby
     "Rakefile"
     "*.gemspec"
     "composer.json"     ; PHP
     "mix.exs"           ; Elixir
     "rebar.config"
     "shard.yml"         ; Crystal
     "dub.json"          ; D
     "dub.sdl"
     "junetion.json"     ; Julia
     "Project.toml"
     "stack.yaml"        ; Haskell
     "cabal.project"
     "*.cabal"
     "pubspec.yaml"      ; Dart
     "Cartfile"          ; Swift (Carthage)
     "Podfile"           ; Swift (CocoaPods)
     "xcodeproj"         ; Swift
     "Pods"
     "meson.build"       ; Meson
     "CMakeLists.txt"    ; CMake
     "CMakeCache.txt"
     "Makefile"
     "configure.ac"
     "configure.in"
     "makefile"
     "scons.mk"
     "SConstruct"
     "webpack.config.js" ; Frontend build tools
     "webpack.config.ts"
     "rollup.config.js"
     "rollup.config.ts"
     "vite.config.js"
     "vite.config.ts"
     "turbo.json"
     "nx.json"
     ".angular"          ; Angular
     "angular.json"
     "nest-cli.json"     ; NestJS
     "Gemfile"
     "Rakefile"))
  :config
  ;; Ensure project.el can find projects using Projectile
  (setq project-switch-commands
        '((?f "Find file" project-find-file)
          (?g "Find regexp" project-find-regexp)
          (?d "Dired" project-dired)
          (?v "VC dir" project-vc-dir)
          (?s "Shell" project-shell))))

;; ============================================
;; Angular Development Environment
;; ============================================

;; TypeScript/TSX mode configuration
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 2))

;; Web mode for HTML templates (including Angular components)
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-engines-alist
        '(("angular" . "\\.component\\.html\\'"))))

;; Vue Mode (lsp-bridge handles LSP via volar)
(use-package vue-mode
  :ensure t
  :mode (("\\.vue\\'" . vue-mode))
  :config
  (setq vue-indent-level 2
        vue-html-tab-width 2))
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
    "mf" '(zig-format-buffer :which-key "Format buffer")))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command (if (executable-find "multimarkdown")
                        "multimarkdown"
                      "markdown"))
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
    "dD" '(dired-jump-other-window :which-key "Jump to dired other window"))
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
(use-package zoxide
  :ensure t
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

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

;;; 09 - Eshell Configuration

(use-package eshell
  :ensure nil
  :custom
  (eshell-scroll-to-bottom-on-input t)
  (tab-always-indent 'complete)
  (eshell-history-size 10000)
  (eshell-save-history-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  :config
  ;; Set up aliases file
  (setq eshell-aliases-file (expand-file-name "eshell/aliases" user-emacs-directory))
  ;; Ensure aliases directory exists
  (unless (file-exists-p (expand-file-name "eshell" user-emacs-directory))
    (make-directory (expand-file-name "eshell" user-emacs-directory) t))
  ;; Create default aliases if file doesn't exist
  (unless (file-exists-p eshell-aliases-file)
    (with-temp-file eshell-aliases-file
      (insert "alias ff find-file $1\n"
              "alias d dired $1\n"
              "alias fd find-dired $PWD \"\"\n"
              "alias clear gemo/eshell-clear-buffer\n"
              "alias ll ls -l $*\n"
              "alias la ls -la $*\n"
              "alias lh ls -lh $*\n")))
  :hook
  (eshell-mode . gemo/eshell-setup)
  :general
  (gemo/leader-keys
    "oe" '(eshell :which-key "Eshell"))
  :bind
  (:map eshell-mode-map
        ("M-m" . beginning-of-line)
        ("M-r" . consult-history)))

;; Fish-style autosuggestions for Eshell
(use-package capf-autosuggest
  :ensure t
  :hook (eshell-mode . capf-autosuggest-mode))

;; Popper for window management
(use-package popper
  :ensure t
  :custom
  (popper-reference-buffers
   '("\\*eshell.*"
     flymake-diagnostics-buffer-mode
     help-mode
     compilation-mode))
  (popper-window-height 15)
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  :general
  (gemo/leader-keys
    "tp" '(popper-toggle :which-key "Toggle popper")
    "tn" '(popper-cycle :which-key "Cycle popper"))
  :bind
  (("C-;" . popper-toggle)
   ("M-;" . popper-cycle)))

;; Eshell helper functions
(defun gemo/eshell-clear-buffer ()
  "Clear current Eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun gemo/shell-create (name)
  "Create named eshell buffer."
  (interactive "sName: ")
  (eshell 'new)
  (let ((new-buffer-name (concat "*eshell-" name "*")))
    (rename-buffer new-buffer-name t)))

;; Abbreviate home directory and shorten path segments
(defun gemo/eshell-abbreviate-path (path)
  "Shorten PATH by abbreviating home dir and truncating segments."
  (let* ((home (expand-file-name "~"))
         (path (replace-regexp-in-string (regexp-quote home) "~" path))
         (segments (split-string path "/"))
         (len (length segments)))
    (if (<= len 3)
        path
      ;; Keep first segment, add ellipsis, keep last 2 segments
      (concat (car segments) "/…/"
              (string-join (last segments 2) "/")))))

;; Virtual environment info (Python, node, etc.)
(defun gemo/eshell-virtual-env-info ()
  "Return virtual environment name if active."
  (let ((venv (or (getenv "VIRTUAL_ENV")      ; Python venv
                  (getenv "CONDA_PREFIX")     ; Conda
                  (getenv "NODE_VIRTUAL_ENV") ; Node
                  (getenv "JENV_SHELL")       ; Java
                  (getenv "RBENV_SHELL"))))   ; Ruby
    (when venv
      (let ((name (file-name-nondirectory (directory-file-name venv))))
        (propertize (format "[%s] " name)
                    'face 'font-lock-constant-face)))))

;; Git branch info for prompt
(defun gemo/eshell-git-branch ()
  "Return git branch name if in a git repository."
  (when (locate-dominating-file default-directory ".git")
    (let ((branch (car (process-lines "git" "branch" "--show-current"))))
      (when branch
        (propertize (format "(%s) " branch)
                    'face 'font-lock-variable-name-face)))))

;; Custom Eshell prompt
(defun gemo/eshell-prompt ()
  "Custom Eshell prompt with zsh-like style."
  (concat
   ;; Virtual environment (if any)
   (gemo/eshell-virtual-env-info)
   ;; User@Host (only when using sudo/remote)
   (when (or (string-match-p "^/sudo:" default-directory)
             (string-match-p "^/ssh:" default-directory))
     (format "%s@%s " (user-login-name) (system-name)))
   ;; Git branch
   (gemo/eshell-git-branch)
   ;; Abbreviated path
   (propertize (gemo/eshell-abbreviate-path (eshell/pwd))
               'face 'font-lock-keyword-face)
   ;; Prompt symbol
   (if (= (user-uid) 0)
       (propertize " # " 'face 'font-lock-warning-face)
     (propertize " λ " 'face 'font-lock-function-name-face))))

;; Left prompt (the main prompt)
(setq eshell-prompt-function 'gemo/eshell-prompt
      eshell-prompt-regexp "^[^#λ]* [#λ] ")

;; Right prompt (optional - shows exit code of last command)
(setq eshell-rprompt-function
      (lambda ()
        (let ((code (eshell-last-command-status)))
          (unless (and (numberp code) (= code 0))
            (propertize (format "[%d]" code)
                        'face 'font-lock-warning-face)))))

(defun gemo/eshell-setup ()
  "Eshell completion setup."
  (setq-local completion-styles '(basic partial-completion))
  (setq-local corfu-auto t)
  (corfu-mode)
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'pcomplete-completions-at-point
                     #'cape-history))))

;;; 99 - Load Local Init
;; Load init.local.el if it exists (for local user configuration)
(let ((init-local (expand-file-name "init.local.el" user-emacs-directory)))
  (when (file-exists-p init-local)
    (load init-local)))

;; Load custom settings
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
