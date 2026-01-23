;;; zig-ts-mode.el --- Tree-sitter support for Zig  -*- lexical-binding: t; -*-

;; Author: Muk
;; Version: 1.0.0
;; Keywords: zig languages tree-sitter
;; URL: https://github.com/tree-sitter-grammars/tree-sitter-zig

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Tree-sitter language versions
;;
;; zig-ts-mode has been tested with the following grammar and version:
;; - tree-sitter-zig: commit 8f513e29f82622dfe37fd8b1e3df95ab57693b5f

;;; Commentary:

;; This package provides major mode for Zig, powered by tree-sitter.
;;
;; Based on:
;; - https://codeberg.org/meow_king/zig-ts-mode
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/c-ts-mode.el
;;
;; To use this mode by default, assuming you have the tree-sitter
;; grammar available, add to your init file:
;;
;;     (add-to-list 'major-mode-remap-alist '(zig-mode . zig-ts-mode))
;;
;; Or customize 'auto-mode-alist':
;;
;;     (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

;; Register Zig language source
(add-to-list
 'treesit-language-source-alist
 '(zig "https://github.com/tree-sitter-grammars/tree-sitter-zig"
       :commit "8f513e29f82622dfe37fd8b1e3df95ab5769b5f")
 t)

;;; Custom variables

(defcustom zig-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `zig-ts-mode'."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'zig)

;;; Syntax table

(defvar zig-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments: Zig only has // comments
    (modify-syntax-entry ?/   ". 12" table)
    (modify-syntax-entry ?\n  ">"    table)

    ;; Strings and Chars
    (modify-syntax-entry ?\"  "\""   table)
    (modify-syntax-entry ?\'  "\""   table)
    (modify-syntax-entry ?\\  "\\"   table)

    ;; Symbol constituents
    (modify-syntax-entry ?_   "_"    table)

    ;; Operators
    (modify-syntax-entry ?+   "."    table)
    (modify-syntax-entry ?-   "."    table)
    (modify-syntax-entry ?=   "."    table)
    (modify-syntax-entry ?%   "."    table)
    (modify-syntax-entry ?&   "."    table)
    (modify-syntax-entry ?|   "."    table)
    (modify-syntax-entry ?^   "."    table)
    (modify-syntax-entry ?!   "."    table)
    (modify-syntax-entry ?@   "."    table)
    (modify-syntax-entry ?~   "."    table)
    (modify-syntax-entry ?<   "."    table)
    (modify-syntax-entry ?>   "."    table)
    (modify-syntax-entry ?*   "."    table)
    (modify-syntax-entry ?.   "."    table)
    table)
  "Syntax table for `zig-ts-mode'.")

;;; Constants

(defconst zig-ts-mode--keywords
  '("asm" "defer" "errdefer" "test" "error" "const" "var"
    "struct" "union" "enum" "opaque"
    "async" "await" "suspend" "nosuspend" "resume"
    "fn"
    "and" "or" "orelse"
    "if" "else" "switch"
    "for" "while" "break" "continue"
    "usingnamespace" "export"
    "try" "catch"
    "volatile" "allowzero" "noalias" "addrspace" "align"
    "callconv" "linksection" "pub"
    "inline" "noinline" "extern" "comptime" "packed" "threadlocal")
  "Zig keywords for tree-sitter font-locking.")

(defconst zig-ts-mode--operators
  '("=" "*=" "*%=" "*|=" "/=" "%=" "+=" "+%=" "+|=" "-=" "-%=" "-|="
    "<<=" "<<|=" ">>=" "&=" "^=" "|=" "!" "~" "-" "-%" "&" "==" "!="
    ">" ">=" "<=" "<" "<<" ">>" "|" "^" "+" "++" "+%" "-%" "+|" "-|"
    "*" "/" "%" "**" "*%" "*|" "||" ".*" ".?" "?" ".." "...")
  "Zig operators for tree-sitter font-locking.")

(defconst zig-ts-mode--font-lock-feature-list
  '(( comment definition)
    ( keyword string type)
    ( builtin constant escape-sequence label number)
    ( bracket delimiter error function operator property variable))
  "Tree-sitter font-lock feature list for Zig.")

;;; Font-lock

(defvar zig-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'zig
   :feature 'comment
   :override t
   '((comment) @font-lock-comment-face)

   :language 'zig
   :feature 'definition
   :override t
   '((function_declaration
      name: (identifier) @font-lock-function-name-face)
     (variable_declaration
      "const" (identifier) @font-lock-constant-face)
     (variable_declaration
      :anchor (identifier) @font-lock-variable-name-face))

   :language 'zig
   :feature 'keyword
   :override t
   `([,@zig-ts-mode--keywords] @font-lock-keyword-face)

   :language 'zig
   :feature 'string
   :override t
   '([(character)
      (string)
      (multiline_string)]
     @font-lock-string-face)

   :language 'zig
   :feature 'builtin
   :override t
   '([(builtin_identifier) "c"] @font-lock-builtin-face
     (calling_convention
      "(" _ @font-lock-builtin-face ")"))

   :language 'zig
   :feature 'constant
   :override t
   '([(boolean) "null" "unreachable" "undefined"]
     @font-lock-constant-face
     (field_expression
      "." member: (identifier) @font-lock-constant-face))

   :language 'zig
   :feature 'label
   :override t
   '((block_label
      (identifier) @font-lock-constant-face)
     (break_label
      (identifier) @font-lock-constant-face))

   :language 'zig
   :feature 'number
   :override t
   '([(integer) (float)] @font-lock-number-face)

   :language 'zig
   :feature 'type
   :override t
   '([(parameter
       type: (identifier) @font-lock-type-face)]
     [(builtin_type) "anyframe"] @font-lock-type-face)

   :language 'zig
   :feature 'bracket
   :override t
   '(["[" "]" "(" ")" "{" "}"] @font-lock-bracket-face
     (payload "|" @font-lock-bracket-face))

   :language 'zig
   :feature 'delimiter
   :override t
   '([";" "." "," ":" "=>" "->"] @font-lock-delimiter-face)

   :language 'zig
   :feature 'function
   :override t
   '((call_expression
      function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (field_expression
                 member: (identifier) @font-lock-function-call-face)))

   :language 'zig
   :feature 'variable
   :override t
   '((field_initializer
      "." (identifier) @font-lock-variable-use-face)
     (field_expression
      (_) member: (identifier) @font-lock-variable-use-face)
     (container_field
      name: (identifier) @font-lock-variable-use-face)
     (identifier) @font-lock-variable-use-face)

   :language 'zig
   :feature 'operator
   :override t
   `([,@zig-ts-mode--operators] @font-lock-operator-face)

   :language 'zig
   :feature 'type
   :override t
   '((enum_declaration
      (container_field
       type: (identifier) @font-lock-type-face)))

   :language 'zig
   :feature 'variable
   :override t
   '((initializer_list
      (assignment_expression
       left: (field_expression
              "." member: (identifier) @font-lock-variable-use-face))))

   :language 'zig
   :feature 'builtin
   :override t
   '(((identifier) @font-lock-builtin-face)
     (:eq? "_" @font-lock-builtin-face))

   :language 'zig
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'zig
   :feature 'constant
   :override t
   '(((identifier) @font-lock-constant-face)
     (:match? "^[A-Z][A-Z_0-9]+$" @font-lock-constant-face))

   :language 'zig
   :feature 'type
   :override t
   '(((identifier) @font-lock-type-face)
     (:match? "^[A-Z_][a-zA-Z0-9_]*" @font-lock-type-face))

   :language 'zig
   :feature 'type
   :override t
   '((variable_declaration
      (identifier) @font-lock-type-face
      "="
      [(struct_declaration) (enum_declaration)
       (union_declaration) (opaque_declaration)]))

   :language 'zig
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for Zig.")

;;; Indent

(defvar zig-ts-mode--indent-rules
  '((zig
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((node-is ";") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((parent-is "multiline_string") prev-line 0)

     ((parent-is "block") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "switch_expression") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "while_statement") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "for_statement") parent-bol zig-ts-mode-indent-offset)

     ((node-is "statement") parent-bol 0)
     (no-node parent-bol 0)))
  "Tree-sitter simple indent rules for Zig.")

;;; Imenu

(defun zig-ts-mode--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (treesit-node-text
   (pcase (treesit-node-type node)
     ("function_declaration"
      (treesit-node-child-by-field-name node "name"))
     ("test_declaration"
      (treesit-node-child node 0 t))
     ((or "struct_declaration" "enum_declaration"
          "union_declaration" "opaque_declaration")
      (when-let ((parent (treesit-node-parent node)))
        (when (equal (treesit-node-type parent) "variable_declaration")
          (treesit-node-child parent 0 t)))))
   t))

(defun zig-ts-mode--defun-valid-p (node)
  "Return non-nil if NODE is a valid defun node.
That is, NODE is not nested."
  (not (treesit-node-top-level
        node
        (rx (or "function_declaration"
               "test_declaration"
               "struct_declaration"
               "enum_declaration"
               "union_declaration"
               "opaque_declaration")))))

;;; Keymap

(defvar-keymap zig-ts-mode-map
  :parent prog-mode-map)

;;; Mode

;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "Zig"
  "Major mode for editing Zig, powered by tree-sitter.

\\{zig-ts-mode-map}

To use tree-sitter Zig mode by default, evaluate

    (add-to-list '\\='major-mode-remap-alist '\\='(zig-mode . zig-ts-mode))

in your init file."
  :syntax-table zig-ts-mode--syntax-table
  :group 'zig

  (when (treesit-ready-p 'zig t)
    (setq-local treesit-primary-parser (treesit-parser-create 'zig))

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx (seq "/" (+ "/") (* (syntax whitespace)))))
    (setq-local comment-multi-line t)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}()[].,;" electric-indent-chars))

    ;; Indent
    (setq-local treesit-simple-indent-rules zig-ts-mode--indent-rules)

    ;; Font-lock
    (setq-local treesit-font-lock-settings zig-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list zig-ts-mode--font-lock-feature-list)

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`function_declaration\\'"
                   #'zig-ts-mode--defun-valid-p
                   #'zig-ts-mode--defun-name)
                  ("Test" "\\`test_declaration\\'"
                   #'zig-ts-mode--defun-valid-p
                   #'zig-ts-mode--defun-name)
                  ("Type" ,(rx bos (or "struct_declaration"
                                       "enum_declaration"
                                       "union_declaration"
                                       "opaque_declaration")
                                 eos)
                   #'zig-ts-mode--defun-valid-p
                   #'zig-ts-mode--defun-name)))

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("function_declaration"
                             "test_declaration"
                             "struct_declaration"
                             "enum_declaration"
                             "union_declaration"
                             "opaque_declaration")))
    (setq-local treesit-defun-name-function #'zig-ts-mode--defun-name)

    (treesit-major-mode-setup)))

(provide 'zig-ts-mode)

;;; zig-ts-mode.el ends here
