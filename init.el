;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;;; 00.1 - Load Options
;; Load default variable definitions
(let ((options (expand-file-name "options.el" user-emacs-directory)))
  (when (file-exists-p options)
    (load options)))

;; Load local configuration overrides
(let ((options-local (expand-file-name "options.local.el" user-emacs-directory)))
  (when (file-exists-p options-local)
    (load options-local)))

;; Load early-init.local.el if it exists (for additional early init)
(let ((early-init-local (expand-file-name "early-init.local.el" user-emacs-directory)))
  (when (file-exists-p early-init-local)
    (load early-init-local)))

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
                  file-name-handler-alist default-file-name-handler-alist)

            ;; Windows-specific optimizations
            (when (eq system-type 'windows-nt)
              ;; Speed up font rendering
              (setq inhibit-compacting-font-cache t)
              ;; Speed up projectile (if installed)
              (setq projectile-git-submodule-command nil))))

;;; 01.1 - Environment Variables (PATH)
;; Ensure Emacs inherits system PATH on all platforms
(defun gemo/inherit-system-path ()
  "Inherit system PATH from shell on all platforms."
  (interactive)
  (let ((path
         (cond
          ;; Windows: Get PATH from PowerShell (both User and System)
          ((eq system-type 'windows-nt)
           (let ((user-path (shell-command-to-string "powershell.exe -NoProfile -Command \"echo [Environment]::GetEnvironmentVariable(\\\"Path\\\", \\\"User\\\")\"")))
             (let ((system-path (shell-command-to-string "powershell.exe -NoProfile -Command \"echo [Environment]::GetEnvironmentVariable(\\\"Path\\\", \\\"Machine\\\")\"")))
               (concat user-path path-separator system-path))))
          ;; Unix-like (macOS/Linux): Get PATH from shell (zsh/bash)
          (t
           (let ((shell (or (getenv "SHELL") "/bin/bash")))
             (shell-command-to-string (concat shell " -l -c 'echo $PATH'")))))))
    ;; Remove trailing newline and set PATH
    (setq path (replace-regexp-in-string "[\r\n]+$" "" path))
    (setenv "PATH" path)
    ;; Update exec-path
    (setq exec-path (append (split-string path path-separator) exec-path))
    ;; Remove duplicates
    (setq exec-path (delete-dups exec-path))))

;; Inherit system PATH at startup
(gemo/inherit-system-path)

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

;; LSP helper functions
(defun gemo/lsp-should-manage-p ()
  "Check if current buffer should be managed by LSP."
  (and (buffer-file-name)
       (not (string-prefix-p " " (buffer-name)))
       (not (string-match-p "markdown-code-fontification" (buffer-name)))
       (not (string-match-p "\\` \\*" (buffer-name)))))

(defun gemo/lsp-ensure-maybe ()
  "Conditionally start LSP based on buffer properties."
  (unless (or (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode 'ron-mode)
              (not (gemo/lsp-should-manage-p)))
    (lsp-deferred)))

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
  :demand t
  :custom
  (projectile-completion-system 'default)  ; Use vertico via completion-at-point
  (projectile-sort-order 'recentf)  ; Sort by recently active
  (projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory))
  (projectile-known-projects-file (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))
  (projectile-project-search-path '("~/projects" "~/work" "~"))  ; Project search paths
  (projectile-globally-ignored-files '(".DS_Store" "*.elc" "node_modules" "target" "zig-cache" "zig-out"))
  (projectile-globally-ignored-directories '(".git" "node_modules" "target" "zig-cache" "zig-out" "vendor" ".venv" "venv" "__pycache__"))
  (projectile-indexing-method 'alien)  ; Faster indexing using external tools
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
          (cons 'transient root))))
    (add-hook 'project-find-functions #'project-try-projectile)))

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
          (?s "Shell" project-shell)
          (?e "Eshell" project-eshell))))

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


(use-package lsp-mode
  :ensure t
  :demand t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-auto-configure t
        lsp-completion-provider :capf
        lsp-prefer-capf t
        lsp-idle-delay 0.3
        lsp-enable-on-type-formatting nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting t
        lsp-enable-imenu t
        lsp-enable-snippet nil
        lsp-keep-workspace-alives nil
        lsp-restart 'auto-restart
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-workspace-folder-watchers nil
        lsp-enable-file-watchers nil
        lsp-enable-text-colors nil
        lsp-enable-indentation nil
        lsp-log-io nil
        lsp-print-performance nil
        lsp-server-trace nil
        lsp-diagnostics-provider :none
        lsp-modeline-diagnostics-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-format-on-save nil
        lsp-before-save-edits nil
        ;; Inlay hint settings
        lsp-inlay-hint-enable t)

  :hook
  (prog-mode . gemo/lsp-ensure-maybe)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . eldoc-box-hover-mode)  ; Enable eldoc-box for LSP
  (lsp-mode . lsp-inlay-hint-mode)   ; Enable inlay hints for type annotations

  :general
  (:states 'normal
           :keymaps 'prog-mode-map
           "gd" #'xref-find-definitions
           "gr" #'xref-find-references
           "gD" #'lsp-find-typeDefinition
           "gR" #'lsp-rename)

  :config
  ;; Language-specific settings
  (lsp-register-custom-settings
   '(("typescript.format.enable" false)
     ("javascript.format.enable" false)))
  )

;; eldoc-box for better documentation display
(use-package eldoc-box
  :ensure t
  :after lsp-mode
  :demand t
  :custom
  (eldoc-box-max-height 20)
  (eldoc-box-max-width 80)
  (eldoc-box-delay 0.3)
  (eldoc-box-only-show-symbol-once t)
  (eldoc-box-clear-with-C-g t)
  :general
  (:states 'normal
           :keymaps 'prog-mode-map
           "K" #'eldoc-box-help-at-point)
  :config
  ;; Disable in terminal mode
  (unless (display-graphic-p)
    (setq eldoc-box-hover-mode nil)))

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

;; ============================================
;; Common Lisp Development (SLIME)
;; ============================================

;; Helper function to find Lisp implementation
(defun gemo/find-lisp-implementation ()
  "Find Common Lisp implementation in PATH."
  (or (executable-find "sbcl")
      (executable-find "ccl")
      (executable-find "clisp")
      (executable-find "ecl")
      (executable-find "abcl")
      "sbcl"))  ; fallback to sbcl (may need to install)

(use-package slime
  :ensure t
  :mode (("\\.cl\\'" . lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.lsp\\'" . lisp-mode))
  :commands slime
  :custom
  ;; Choose your Lisp implementation
  (inferior-lisp-program (gemo/find-lisp-implementation))
  (slime-lisp-implementations
   `(;; Try to find SBCL in common locations
     (sbcl (,(or (executable-find "sbcl") "/opt/homebrew/bin/sbcl" "/usr/local/bin/sbcl" "sbcl"))
            :coding-system utf-8-unix)
     (sbcl-mt (,(or (executable-find "sbcl") "/opt/homebrew/bin/sbcl" "/usr/local/bin/sbcl" "sbcl")
               "--dynamic-space-size" "4096")
            :coding-system utf-8-unix)
     (ccl (,(or (executable-find "ccl") "/usr/local/bin/ccl" "ccl"))
          :coding-system utf-8-unix)
     (clisp (,(or (executable-find "clisp") "/usr/bin/clisp" "clisp")
            "-K" "full")
            :coding-system utf-8-unix)))
  (slime-default-lisp 'sbcl)
  ;; SLIME behavior
  (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (slime-enable-evaluate-in-emacs t)
  (slime-export-symbol-representation-auto t)
  (slime-repl-return-behaviour :send-only-if-complete)
  (slime-autodoc-use-multiline-p t)
  (slime-description-autofocus t)
  ;; UI improvements
  (slime-compilation-finished-hook 'slime-maybe-show-compilation-log)
  (slime-repl-history-file (expand-file-name ".slime-history" user-emacs-directory))
  (slime-repl-history-remove-duplicates t)
  :config
  (slime-setup '(slime-fancy
                 slime-fuzzy
                 slime-indentation
                 slime-sbcl-exts
                 slime-repl
                 slime-autodoc
                 slime-tramp
                 slime-asdf))
  ;; Keybindings for SLIME REPL
  :general
  (:keymaps 'slime-repl-mode-map
            "C-c C-z" #'switch-to-buffer
            "C-c C-y" #'slime-repl-yank)
  (gemo/leader-keys
    "l"  '(:ignore t :which-key "lisp")
    "ls" '(slime-selector :which-key "SLIME selector")
    "li" '(slime :which-key "Start SLIME")
    "lr" '(slime-reset-connection :which-key "Reset connection")
    "lc" '(slime-interrupt :which-key "Interrupt")
    "lq" '(slime-quit-lisp :which-key "Quit SLIME")))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

;;; 99 - Load Local Init
;; Load init.local.el if it exists (for local user configuration)
(let ((init-local (expand-file-name "init.local.el" user-emacs-directory)))
  (when (file-exists-p init-local)
    (load init-local)))

;;; init.el ends here
