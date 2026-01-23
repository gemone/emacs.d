;;; coding-mode.el --- Tree-sitter configuration for various programming languages -*- lexical-binding: t; -*-

;; Author: Muk
;; Version: 2.2.0
;; Keywords: convenience, languages, tree-sitter

;;; Commentary:

;; This file configures tree-sitter support for Emacs 29+ following
;; the official best practices from the GNU Emacs Lisp Reference Manual.
;;
;; Based on:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html
;; - https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode
;;
;; Key Features:
;; - Automatic mode remapping for tree-sitter modes
;; - Proper file type associations
;; - Safe loading without warnings
;; - Helper functions for grammar management

;;; Code:

;;; ============================================================
;;; TREE-SITTER LANGUAGE SOURCES
;;; ============================================================

;; Define tree-sitter grammar sources for all supported languages.
;; This list is used by `treesit-install-language-grammar'.
;; Organized alphabetically for easier maintenance.
;;
;; Note: emacs-lisp uses the built-in mode which doesn't require
;; tree-sitter support.
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

;; Add user's tree-sitter directory to search path
;; This allows Emacs to find manually compiled grammars
(add-to-list 'treesit-extra-load-path
             (expand-file-name "tree-sitter" user-emacs-directory))

;;; ============================================================
;;; MAJOR MODE REMAPPING
;;; ============================================================

;; Configure major mode remapping for tree-sitter modes.
;; Following the best practice: use `major-mode-remap-alist' to automatically
;; use tree-sitter modes when grammars are available. Emacs will gracefully
;; fall back to traditional modes if tree-sitter is not available.
;;
;; This is the recommended approach for languages with built-in tree-sitter
;; support in Emacs 29+. See:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html

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
           (js-mode          . js-ts-mode)
           (js2-mode         . js-ts-mode)
           (js-json-mode     . json-ts-mode)
           (makefile-mode    . makefile-ts-mode)
           (python-mode      . python-ts-mode)
           (typescript-mode  . typescript-ts-mode)
           (yaml-mode        . yaml-ts-mode)
           (zig-mode         . zig-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

;;; ============================================================
;;; FILE TYPE ASSOCIATIONS
;;; ============================================================

;; Special file type associations that need explicit configuration.
;; Most file types are handled automatically by the major mode remapping,
;; but some files require explicit `auto-mode-alist' entries.

;; Web Technologies
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;; CMake (build configuration)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\.in\\'" . cmake-ts-mode))

;; Go modules
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.sum\\'" . go-mod-ts-mode))

;; Zig
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
(add-to-list 'auto-mode-alist '("\\.zig\\.zon\\'" . zig-ts-mode))

;;; ============================================================
;;; LOAD CUSTOM TREE-SITTER MODES
;;; ============================================================

;; Load custom tree-sitter modes from progmodes subdirectory
(let ((progmodes-dir (expand-file-name "progmodes" (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p progmodes-dir)
    (add-to-list 'load-path progmodes-dir)
    (require 'zig-ts-mode nil 'no-error)))

;;; ============================================================
;;; HELPER FUNCTIONS
;;; ============================================================

;;;###autoload
(defun gemo/treesit-supported-p ()
  "Check if current Emacs supports tree-sitter.
Returns non-nil if Emacs was built with tree-sitter support."
  (interactive)
  (if (treesit-available-p)
      (progn
        (message "✓ Tree-sitter is supported (Emacs %d.%d)"
                 emacs-major-version emacs-minor-version)
        t)
    (progn
      (message "✗ Tree-sitter not available in this Emacs build")
      nil)))

;;;###autoload
(defun gemo/treesit-check-grammars ()
  "Check which tree-sitter grammars are installed.
Displays a checklist of installed and missing grammars."
  (interactive)
  (when (gemo/treesit-supported-p)
    (let ((installed 0)
          (missing 0))
      (dolist (lang (mapcar 'car treesit-language-source-alist))
        (if (treesit-ready-p lang t)  ; t = quiet, no warnings
            (progn
              (message "✓ %s" lang)
              (cl-incf installed))
          (progn
            (message "✗ %s" lang)
            (cl-incf missing))))
      (message "\nSummary: %d installed, %d missing" installed missing)
      (when (> missing 0)
        (message "Run `M-x gemo/treesit-install-missing-grammars' to install missing grammars.")))))

;;;###autoload
(defun gemo/treesit-install-grammar (lang)
  "Install a single tree-sitter grammar for LANG.
Interactively prompts for the language to install."
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
  "Install all missing tree-sitter grammars.
Attempts to install each language grammar that is not currently available."
  (interactive)
  (when (gemo/treesit-supported-p)
    (let ((missing 0)
          (failed 0))
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
          (message "Installation attempt complete. %d succeeded, %d failed."
                   (- missing failed) failed)
        (message "All grammars are already installed!")))))

(provide 'coding-mode)

;;; coding-mode.el ends here
