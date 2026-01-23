;;; lsp-mode.el --- Eglot LSP configuration for Emacs 29+ -*- lexical-binding: t; -*-

;; Author: Muk
;; Version: 1.0.0
;; Keywords: convenience, languages, lsp

;;; Commentary:

;; This file configures Eglot (Emacs 29+ built-in LSP client) for Java,
;; Python, TypeScript/JavaScript, Angular, and HTML development.
;;
;; ============================================================
;; LANGUAGE SERVER INSTALLATION
;; ============================================================
;; Eglot requires language servers to be installed manually. Run these
;; commands in your terminal to install the required servers:
;;
;;   # Java - Eclipse JDT Language Server
;;   brew install jdtls
;;
;;   # Python - Pyright
;;   brew install pyright
;;
;;   # TypeScript/JavaScript
;;   npm install -g typescript typescript-language-server
;;
;;   # HTML
;;   npm install -g vscode-langservers-extracted
;;
;;   # Angular (optional, for Angular projects)
;;   npm install -g @angular/language-server
;;
;; ============================================================
;; MULTI-LSP SUPPORT (RASSUMFRASSUM)
;; ============================================================
;; Rassumfrassum is an LSP multiplexer that allows connecting Eglot to
;; multiple LSP servers simultaneously. This is useful for:
;; - Python: ty + ruff (type checking + fast linting)
;; - TypeScript: tsserver + eslint
;; - Vue: VLS + tailwindcss-language-server
;;
;; Installation:
;;   pip install rassumfrassum
;;
;; Usage: Open a file and run:
;;   M-x eglot
;;   Then enter: rass python    (for Python with ty+ruff)
;;   Or:         rass ts        (for TypeScript with tsserver+eslint)
;;
;; Presets available: python, basedruff, ts, vue
;; See https://github.com/joaotavora/rassumfrassum for details
;;
;; ============================================================
;; Key Features:
;; - Corfu for in-buffer completion with documentation in echo area
;; - Language server configurations for multiple languages
;; - Evil-mode compatible keybindings via general.el
;; - Helper functions for LSP operations
;; - Integration with existing tree-sitter modes
;;
;; Dependencies:
;; - Eglot (built-in to Emacs 29+)
;; - Corfu (completion UI)
;; - General.el (keybindings)

;;; Code:

;;; ============================================================
;;; CORFU CONFIGURATION
;;; ============================================================

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; Enable auto completion
  (corfu-auto-delay 0.2)         ; Delay for auto completion
  (corfu-auto-prefix 2)          ; Start completion after 2 characters
  (corfu-preselect 'prompt)      ; Preselect the prompt
  (corfu-on-exact-match nil)     ; Configure handling of exact matches
  :general
  (:keymaps 'corfu-map
            "C-j" #'corfu-next
            "C-k" #'corfu-previous
            "<escape>" #'corfu-quit))

;;; ============================================================
;;; EGLOT CONFIGURATION
;;; ============================================================

(use-package eglot
  :ensure nil  ; Built-in to Emacs 29+
  :config
  ;; Reduce Eglot noise in the echo area
  (setq eglot-events-buffer-size 0)  ; Disable events buffer
  (setq eglot-connect-timeout 30)
  (setq eglot-sync-timeout 5)

  ;; Cache directory for LSP workspaces
  (defvar gemo/lsp-cache-dir
    (expand-file-name ".cache/" user-emacs-directory)
    "Directory for LSP server caches.")

  (unless (file-exists-p gemo/lsp-cache-dir)
    (make-directory gemo/lsp-cache-dir t))

  ;; ============================================================
  ;; Language Server Configurations
  ;; ============================================================

  ;; Java - Eclipse JDT Language Server
  (defun gemo/eglot-java-server ()
    "Return the Java language server command."
    (list "jdtls"
          "-data" (expand-file-name "jdtls-workspace" gemo/lsp-cache-dir)))

  ;; Python - Pyright
  (defun gemo/eglot-python-server ()
    "Return the Python language server command."
    (list "pyright-langserver"
          "--stdio"))

  ;; TypeScript/JavaScript
  (defun gemo/eglot-typescript-server ()
    "Return the TypeScript language server command."
    (list "typescript-language-server"
          "--stdio"))

  ;; HTML
  (defun gemo/eglot-html-server ()
    "Return the HTML language server command."
    (list "vscode-html-language-server"
          "--stdio"))

  ;; Angular - Auto-detect based on angular.json presence
  (defun gemo/eglot-angular-server-p (&optional _)
    "Check if current directory is an Angular project."
    (and (locate-dominating-file default-directory "angular.json")
         (list "ngserver"
               "--stdio"
               "--tsProbeLocations"
               (list (expand-file-name "node_modules/typescript/lib" default-directory)
                     (expand-file-name "lib" (or (getenv "NVM_DIR") ""))
                     "/usr/local/lib/node_modules/typescript/lib")
               "--ngProbeLocations"
               (list (expand-file-name "node_modules/@angular/language-server" default-directory)
                     "/usr/local/lib/node_modules/@angular/language-server"))))

  ;; ============================================================
  ;; Eglot Server Programs
  ;; ============================================================

  ;; Configure language servers for specific modes
  (add-to-list 'eglot-server-programs
               `(java-ts-mode . gemo/eglot-java-server))
  (add-to-list 'eglot-server-programs
               `(java-mode . gemo/eglot-java-server))

  (add-to-list 'eglot-server-programs
               `(python-ts-mode . gemo/eglot-python-server))
  (add-to-list 'eglot-server-programs
               `(python-mode . gemo/eglot-python-server))

  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode . gemo/eglot-typescript-server))
  (add-to-list 'eglot-server-programs
               `(tsx-ts-mode . gemo/eglot-typescript-server))
  (add-to-list 'eglot-server-programs
               `(js-ts-mode . gemo/eglot-typescript-server))

  (add-to-list 'eglot-server-programs
               `(html-ts-mode . gemo/eglot-html-server))

  ;; Angular (conditional activation)
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode html-ts-mode)
                 . gemo/eglot-angular-server-p))

  ;; ============================================================
  ;; Keybindings (Code Actions)
  ;; ============================================================

  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   :prefix "SPC"

   ;; Code actions (SPC c ...)
   "c"  '(:ignore t :hint "code")
   "ca" '(eglot-code-actions :hint "Code actions")
   "cr" '(eglot-rename :hint "Rename symbol")
   "cf" '(eglot-format-buffer :hint "Format buffer")
   "cI" '(eglot-code-action-organize-imports :hint "Organize imports")

   ;; Navigation (SPC g ...)
   "g"  '(:ignore t :hint "goto")
   "gd" '(xref-find-definitions :hint "Go to definition")
   "gr" '(xref-find-references :hint "Find references")
   "gi" '(imenu :hint "Jump to symbol in buffer")
   "gI" '(eglot-find-workspace-symbol :hint "Find workspace symbol")

   ;; Help/Documentation (SPC h ...)
   "h"  '(:ignore t :hint "help")
   "hh" '(eldoc-doc-buffer :hint "Show documentation")
   "hs" '(eglot-signature-eldoc-help :hint "Signature help")

   ;; Workspace (SPC s ...)
   "s"  '(:ignore t :hint "search")
   "sw" '(eglot-find-workspace-symbol :hint "Search workspace symbols"))

  ;; ============================================================
  ;; Global LSP Keybindings
  ;; ============================================================

  ;; Rassumfrassum multi-LSP keybindings (SPC L ...)
  (gemo/leader-keys
    "L"  '(:ignore t :hint "LSP (multi-server)")
    "Lp" '(gemo/eglot-rass-python :hint "Rass: Python (ty+ruff)")
    "Lt" '(gemo/eglot-rass-ts :hint "Rass: TypeScript (tsserver+eslint)")
    "Lv" '(gemo/eglot-rass-vue :hint "Rass: Vue (VLS+tailwind)")
    "LR" '(gemo/eglot-rass-basedruff :hint "Rass: basedruff (basedpyright+ruff)"))

  ;; Global navigation keybindings (work with or without LSP)
  (general-define-key
   :states 'normal
   "gd" #'xref-find-definitions
   "gr" #'xref-find-references
   "gi" #'imenu
   "gI" #'project-find-symbol)

  ;; ============================================================
  ;; Hook Configuration
  ;; ============================================================

  ;; Auto-start Eglot for supported modes
  ;; Note: Eglot will silently fail if language servers are not installed
  ;; See installation instructions at the top of this file
  (dolist (hook '(java-ts-mode-hook
                  python-ts-mode-hook
                  typescript-ts-mode-hook
                  tsx-ts-mode-hook
                  js-ts-mode-hook
                  html-ts-mode-hook))
    (add-hook hook #'eglot-ensure)))

;;; ============================================================
;;; HELPER FUNCTIONS
;;; ============================================================

;;;###autoload
(defun gemo/lsp-capabilities ()
  "Display current LSP server capabilities."
  (interactive)
  (if (bound-and-true-p eglot--managed-mode)
      (let ((server (eglot-current-server)))
        (if server
            (with-help-window "*LSP Capabilities*"
              (princ (json-serialize (json-plist->object
                                     (eglot--server-capabilities server))))
              (help-print-return-message))
          (message "No active LSP server")))
    (message "Eglot not active in this buffer")))

;;;###autoload
(defun gemo/lsp-restart ()
  "Restart LSP server for current buffer."
  (interactive)
  (if (bound-and-true-p eglot--managed-mode)
      (progn
        (eglot-shutdown)
        (eglot-ensure)
        (message "LSP server restarted"))
    (message "Eglot not active in this buffer. Run M-x eglot to start.")))

;;;###autoload
(defun gemo/lsp-info ()
  "Show information about active LSP servers."
  (interactive)
  (let ((servers (eglot--all-servers)))
    (if servers
        (with-help-window "*LSP Servers*"
          (princ "Active LSP Servers:\n\n")
          (dolist (server servers)
            (princ (format "Project: %s\n" (eglot--project-root server)))
            (princ (format "Server: %s\n" (eglot--server-contact server))))
          (help-print-return-message))
      (message "No active LSP servers"))))

;;; ============================================================
;;; RASSUMFRASSUM MULTI-LSP HELPERS
;;; ============================================================

;;;###autoload
(defun gemo/eglot-rass-python ()
  "Start Eglot with rassumfrassum Python preset (ty + ruff)."
  (interactive)
  (eglot '("rass" "python")))

;;;###autoload
(defun gemo/eglot-rass-ts ()
  "Start Eglot with rassumfrassum TypeScript preset (tsserver + eslint)."
  (interactive)
  (eglot '("rass" "ts")))

;;;###autoload
(defun gemo/eglot-rass-vue ()
  "Start Eglot with rassumfrassum Vue preset (VLS + tailwindcss)."
  (interactive)
  (eglot '("rass" "vue")))

;;;###autoload
(defun gemo/eglot-rass-basedruff ()
  "Start Eglot with rassumfrassum basedruff preset (basedpyright + ruff)."
  (interactive)
  (eglot '("rass" "basedruff")))

(provide 'lsp-mode)

;;; lsp-mode.el ends here
