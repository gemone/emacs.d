;;; init.el

;;; ============================================================
;;; 00 - PERFORMANCE
;;; ============================================================
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000) ; Set to ~2MB
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

;;; ============================================================
;;; 01 - PACKAGE MANAGER
;;; ============================================================
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

;;; ============================================================
;;; 02 - UI SETTINGS
;;; ============================================================
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

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;; ============================================================
;;; 04 - KEYBINDINGS (EVIL & GENERAL)
;;; ============================================================
(use-package evil
  :ensure t
  :demand t
  :init
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

  (gemo/leader-keys
    "SPC" '(execute-extended-command :hint "M-x")
    "!"   '(shell-command :hint "Shell command")
    
    ;; Buffer Management
    "b"  '(:ignore t :hint "buffer")
    "bb" '(switch-to-buffer :hint "Switch buffer")
    "bk" '(kill-current-buffer :hint "Kill buffer")
    "bn" '(next-buffer :hint "Next buffer")
    "bp" '(previous-buffer :hint "Prev buffer")
    "br" '(revert-buffer :hint "Revert buffer")
    )
  )

(use-package which-key
  :ensure t
  :demand t
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-secondary-delay 0.1)
  (which-key-mode))

;;; ============================================================
;;; 03 - FONT CONFIGURATION
;;; ============================================================
(use-package font
  :load-path "lisp"
  :demand t)

(use-package unicad
  :ensure t
  :demand t
  :config (unicad-mode))

;;; ============================================================
;;; 05 - COMPLETION
;;; ============================================================
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :general
  (:keymaps 'vertico-map
	    "C-j" #'vertico-next
	    "C-k" #'vertico-previous))

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
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;;; ============================================================
;;; 06 - TOOLS
;;; ============================================================
(use-package consult
  :ensure t
  :general
  (gemo/leader-keys
    "s"  '(:ignore t :hint "search")
    "ss" '(consult-line :hint "Search lines")
    "si" '(consult-imenu :hint "Jump to symbol")
    "so" '(consult-outline :hint "Search outline/symbols") ; Quick jump in file
    ))

(use-package transient :ensure t)

(use-package magit
  :ensure t
  :after transient
  :general
  (gemo/leader-keys
    "g"  '(:ignore t :hint "git")
    "gg" '(magit-status :hint "Magit status")))


;;; ============================================================
;;; 07 - CODING (TREE-SITTER)
;;; ============================================================
;; Load tree-sitter configuration for various programming languages
;; See lisp/coding-mode.el for detailed documentation
(use-package coding-mode
  :load-path "lisp"
  :demand t)


;;; TODO: custom.el
