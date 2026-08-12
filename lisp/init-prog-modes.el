;;; init-prog-modes.el --- opt-in prog-mode framework + custom.el -*- lexical-binding: t; -*-

;;; Commentary:
;; The opt-in `prog-mode' language framework.  Languages are enabled by
;; listing their symbol in `my/install-prog-modes' (set in `custom.el');
;; `M-x my/add-prog-modes' toggles the list through the minibuffer and
;; persists it.  This module also defines the extensible hook variables
;; consumed by the frontend language modules (`my/frontend-ts-contacts',
;; `my/frontend-web-contacts', `my/web-mode-auto-mode') and loads the
;; user's `custom.el' so those variables can be extended there.
;;
;; Load order matters: `my/install-prog-modes' and the contact hooks must
;; be defined before `custom.el' is loaded, so this module is loaded
;; right after `init-core'.  Loaded from `init.el' as `init-prog-modes'.

;;; Code:
;; Opt-in prog-mode languages. A language's `use-package' form is gated by
;; `:if (memq 'SYM my/install-prog-modes)'; default nil installs none.
;; Populate in custom.el (see custom-example.el).
(defvar my/install-prog-modes nil
  "List of language symbols whose packages elpaca should install.
`init.el' loads each `init-lang-*' module only when its language symbol
is a member (see `my/lang-module-specs'), so disabled languages are never
loaded and their packages never installed.  nil means install none.
Example: (setq my/install-prog-modes '(zig)).")

;; Predicate used by `init.el' to decide which `init-lang-*' modules to
;; load: each module is loaded only when its language is enabled, so
;; disabled languages are never loaded at all.  The opt-in list itself is
;; maintained automatically via `M-x my/add-prog-modes'.

(defun my/prog-mode-enabled-p (lang)
  "Return non-nil if LANG (a symbol) is in `my/install-prog-modes'."
  (memq lang my/install-prog-modes))

;; User-tunable extension variables.  These are defined HERE, before
;; `custom-file' is loaded just below, so that custom.el can extend them at
;; load time with `add-hook' / `add-to-list'.  Their consumers (the LSP
;; dispatchers `my/ts-ls-contact' / `my/web-ls-contact' and the web-mode
;; `auto-mode-alist' wiring) live further down and read whatever custom.el
;; has set.  See custom-example.el for what each one does and how to extend.
(defvar my/frontend-ts-contacts nil
  "Abnormal hook of TypeScript LSP resolvers for the current project.
Each function takes no arguments and returns an eglot contact
\(command list) when it wants to handle the current project, or nil.
`my/ts-ls-contact' tries them in order until one returns non-nil.
Framework blocks (Angular, Vue, ...) add resolvers here; extend from
custom.el with `add-hook'.")

(defvar my/frontend-web-contacts nil
  "Abnormal hook of HTML/web LSP resolvers for the current buffer.
Same contract as `my/frontend-ts-contacts' but consulted for
`html-mode' / `html-ts-mode' / `web-mode' buffers.")

(defvar my/web-mode-auto-mode
  '("\\.phtml\\'"     "\\.tpl\\.php\\'" "\\.[agj]sp\\'"  "\\.as[cp]x\\'"
    "\\.erb\\'"       "\\.mustache\\'"  "\\.ejs\\'"      "\\.djhtml\\'"
    "\\.jinja\\'"     "\\.j2\\'"        "\\.php\\'")
  "List of `auto-mode-alist' regexps for files opened in `web-mode'.
Extend it from custom.el with `add-to-list' (see custom-example.el).
Plain .html is intentionally excluded so `html-ts-mode' handles it.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)

;;; --- Interactive prog-mode selector ---
;; `M-x my/add-prog-modes' selects languages through the standard minibuffer
;; completion UI (the same vertico/orderless popup as `M-x'), via
;; `completing-read-multiple'; each candidate is annotated with its
;; description.  The selection becomes the new `my/install-prog-modes'
;; (default = current set), is written to `custom.el', and init.el is
;; reloaded so newly enabled languages install/load.  The selector
;; (`my/select-multi') is generic: reuse it for any (KEY . LABEL) alist
;; ("other options likewise").

(defconst my/prog-mode-catalog
  '((zig          . "zig-mode (Zig)")
    (common-lisp  . "slime (Common Lisp, SBCL)")
    (java         . "eglot-java + dape + java-server (Java stack)")
    (web-basic    . "web-mode + TypeScript/HTML LSP (basic web)")
    (web-vue      . "Volar (@vue/language-server) (.vue SFCs)")
    (web-angular  . "@angular/language-server (ngserver)")
    (markdown     . "markdown-mode + edit-indirect"))
  "Alist (SYMBOL . LABEL) of opt-in prog-mode languages.
Each SYMBOL gates the matching `init-lang-*' module, which `init.el'
loads only when the symbol is enabled.")

(defun my/select-multi--affix (by-name picked done)
  "Return an affixation function for `my/select-multi''s completion.
BY-NAME maps candidate name (string) -> label (string).  PICKED is the
list of currently selected name strings; DONE is the finish sentinel.
Each candidate is prefixed `[x]' (selected) or `[ ]' and suffixed with
its label; DONE is suffixed `-- finish selection'.  All affixes are
strings (never symbols), so `concat'/vertico never choke."
  (lambda (cs)
    (mapcar (lambda (c)
              (let ((label (and (not (equal c done))
                                (cdr (assoc c by-name)))))
                (list c
                      (cond ((equal c done)    "    ")
                            ((member c picked) "[x] ")
                            (t                 "[ ] "))
                      (cond ((equal c done) "  -- finish selection")
                            (label           (concat "  " label))
                            (t               "")))))
            cs)))

(defun my/select-multi (prompt entries current)
  "Toggle-select multiple KEYs from ENTRIES, one per round, and return them.
Each round is a normal `completing-read' (the M-x-style vertico/orderless
popup), so multi-selection is robust and does not depend on
`completing-read-multiple'.  PROMPT is the prefix shown in the minibuffer
prompt each round.  Pick a candidate to TOGGLE it on/off
\(selected ones are marked `[x]'); pick `== done ==' to finish.  ENTRIES is
a list of (KEY . LABEL) where KEY is a symbol and LABEL a string.  CURRENT
is the initial selection.  Returns the final list of selected KEYs
\(symbols)."
  (let* ((by-name (mapcar (lambda (e) (cons (symbol-name (car e)) (cdr e)))
                          entries))
         (cands (mapcar #'car by-name))
         (done "== done ==")
         ;; `done' is always the LAST candidate, so the finish sentinel
         ;; stays at the bottom of the vertico list.
         (all (append cands (list done)))
         (picked (mapcar #'symbol-name (copy-sequence current))))
    (catch 'done
      (while t
        (let* ((completion-extra-properties
                (list :affixation-function
                      (my/select-multi--affix by-name picked done)))
               (choice (completing-read
                        (format "%s[%d selected] pick to toggle, `%s' to finish: "
                                prompt (length picked) done)
                        all nil t)))
          (cond
           ((equal choice done)    (throw 'done nil))
           ((member choice picked) (setq picked (delete choice picked)))
           (t                      (push choice picked))))))
    (delq nil (mapcar (lambda (s)
                        (let ((sym (intern-soft s)))
                          (and sym (assq sym entries) sym)))
                      picked))))

(defun my/prog-modes--write-custom (modes)
  "Persist `my/install-prog-modes' = MODES into the variable `custom-file'.
Replaces the existing setq line, or inserts one before the footer;
creates the variable `custom-file' if it is absent."
  (let* ((file (or custom-file
                   (expand-file-name "custom.el" user-emacs-directory)))
         (line (format "(setq my/install-prog-modes '%S)" modes))
         (body (if (file-exists-p file)
                   (with-temp-buffer
                     (insert-file-contents file)
                     (goto-char (point-min))
                     (if (re-search-forward
                          "^[ \t]*(setq[ \t]+my/install-prog-modes[ \t]+'.*)[ \t]*$"
                          nil 'noerror)
                         (progn (replace-match line) (buffer-string))
                       (let ((pos (or (save-excursion
                                        (goto-char (point-min))
                                        (and (re-search-forward "^(provide" nil t)
                                             (line-beginning-position)))
                                      (point-max))))
                         (goto-char pos)
                         (insert line "\n\n")
                         (buffer-string))))
                 (concat ";;; custom.el --- your custom el -*- lexical-binding: t; -*-\n\n"
                         line "\n\n(provide 'custom)\n\n;;; custom.el ends here\n"))))
    (with-temp-file file
      (insert body))))

(defun my/prog-modes--reload-init ()
  "Reload the configuration modules and flush elpaca's queue so new prog-modes take effect.
`my/reload-config' re-evaluates every `init-*' module (excluding the
`init-package' bootstrap) so newly enabled languages are loaded; the
elpaca queue is then processed to build/install any that are new."
  (condition-case-unless-debug err
      (progn
        (my/reload-config)
        (when (fboundp 'elpaca-process-queues)
          (elpaca-process-queues))
        (message "init.el reloaded; prog-modes applied."))
    (error
     (message "Reload failed: %s" (error-message-string err))
     (message "Saved to custom.el; restart Emacs to apply fully."))))

(defun my/prog-modes--apply (modes)
  "Persist MODES as `my/install-prog-modes', then reload init.el.
MODES (a list of symbols) REPLACES the current enabled set.  Paired
with `my/select-multi', which returns the full toggled set, so toggling
a language off removes it."
  (let ((sorted (sort (copy-sequence modes)
                      (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
    (my/prog-modes--write-custom sorted)
    (setq my/install-prog-modes sorted)
    (message "my/install-prog-modes => %S" sorted)
    (my/prog-modes--reload-init)))

(defun my/add-prog-modes ()
  "Toggle `prog-mode' languages on/off via the minibuffer and apply them.
Each round pops up the standard M-x-style completion (vertico/orderless);
pick a language to toggle it on/off (selected ones are marked `[x]'), and
pick `== done ==' to finish.  The resulting set is written to `custom.el'
as `my/install-prog-modes' and init.el is reloaded so the change takes
effect (newly enabled languages install/load)."
  (interactive)
  (my/prog-modes--apply
   (my/select-multi "Prog modes: "
                    my/prog-mode-catalog
                    my/install-prog-modes)))

(provide 'init-prog-modes)

;;; init-prog-modes.el ends here
