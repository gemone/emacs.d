;;; init-lang-web.el --- frontend: TS/Angular/Vue/web-mode LSP -*- lexical-binding: t; -*-

;;; Commentary:
;; Frontend language support.  Each concern is an independent opt-in
;; symbol in `my/install-prog-modes' (`typescript', `angular', `vue',
;; `web-mode'); see the selector in `init-prog-modes'.  Eglot allows one
;; server per buffer, so each mode family (TS, html/web) has ONE contact
;; dispatcher (`my/ts-ls-contact' / `my/web-ls-contact') that consults the
;; extensible hooks `my/frontend-ts-contacts' / `my/frontend-web-contacts'
;; (defined in `init-prog-modes', extendable from custom.el).  Add a
;; framework by pushing a resolver onto the relevant hook.

;;; Code:
;;; --- Frontend LSP: TypeScript / Angular / Vue / web-mode ---
;; Each frontend concern is an INDEPENDENT opt-in symbol in
;; `my/install-prog-modes':
;;   `typescript'  -- typescript-language-server for .ts/.tsx
;;                    (built-in typescript-ts-mode / tsx-ts-mode; no package)
;;   `angular'     -- @angular/language-server (ngserver) for Angular projects
;;   `vue'         -- Volar (@vue/language-server) for .vue SFCs
;;   `web-mode'    -- web-mode package + generic HTML/web LSP server
;;
;; Eglot allows one server per buffer, so each mode family (TS, html/web)
;; has ONE contact dispatcher that consults an extensible hook of project
;; resolvers (`my/frontend-ts-contacts' / `my/frontend-web-contacts').
;; The first resolver returning non-nil wins; otherwise the stock server is
;; used.  Add a framework by pushing a resolver onto the relevant hook --
;; no need to touch the eglot entries.
;;
;; typescript-ts-mode / tsx-ts-mode / html-ts-mode are built-in (Emacs 29+)
;; and autoloaded, so plain symbol references suffice; no :ensure.
;; typescript-mode is the separate GNU ELPA package; it never matches when
;; absent.

;; `my/frontend-ts-contacts' / `my/frontend-web-contacts' are defined early
;; (before `custom-file' loads) so custom.el can extend them; the
;; dispatchers below just consult those hooks.

(defun my/ts-ls-contact (&optional _interactive)
  "Return the TypeScript LSP contact for the current project.
Framework-aware via `my/frontend-ts-contacts'; defaults to
`typescript-language-server'."
  (or (run-hook-with-args-until-success 'my/frontend-ts-contacts)
      '("typescript-language-server" "--stdio")))

(defun my/web-ls-contact (&optional _interactive)
  "Return the HTML/web LSP contact for the current buffer.
Framework-aware via `my/frontend-web-contacts'; defaults to
`vscode-html-language-server'."
  (or (run-hook-with-args-until-success 'my/frontend-web-contacts)
      (eglot-alternatives
       '(("vscode-html-language-server" "--stdio")
         ("html-languageserver" "--stdio")))))

;;; --- TypeScript (built-in typescript-ts-mode / tsx-ts-mode) ---
;; No package is installed; only the LSP wiring is opt-in.  The entry is
;; registered via `with-eval-after-load' so it is present before the first
;; connect, independent of any package load.
(when (memq 'typescript my/install-prog-modes)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(((typescript-ts-mode :language-id "typescript")
                    (typescript-mode  :language-id "typescript")
                    (tsx-ts-mode      :language-id "typescriptreact"))
                   . my/ts-ls-contact))))

;;; --- web-mode (independent, extensible) ---
;; web-mode edits mixed-content web templates: HTML with embedded CSS, JS,
;; and template engines (PHP, Django/Jinja, ERB, JSP, Mustache, EJS, ...).
;; Its two core facets are made explicit and extensible here:
;;   * `my/web-mode-auto-mode'          -- file extensions that open in web-mode
;;   * `web-mode-enable-engine-detection' -- detect the template engine
;; Extend `my/web-mode-auto-mode' in custom.el to add file types, e.g.
;;   (add-to-list 'my/web-mode-auto-mode "\\.twig\\'")
;; Plain .html is intentionally NOT in the list so html-ts-mode
;; (treesit-auto) keeps handling it; add it there if you prefer web-mode.

;; `my/web-mode-auto-mode' is defined early (before `custom-file' loads) so
;; custom.el can extend it with `add-to-list'.

;; Wire the extensions into `auto-mode-alist' at init (independent of when
;; web-mode loads), so opening one of these files autoloads web-mode.
(when (memq 'web-mode my/install-prog-modes)
  (dolist (re my/web-mode-auto-mode)
    (add-to-list 'auto-mode-alist (cons re 'web-mode))))

(use-package web-mode
  :ensure t
  :if (memq 'web-mode my/install-prog-modes)
  ;; auto-mode-alist wiring above autoloads web-mode on file open.
  :defer t
  :custom
  ;; Indentation (2 spaces; matches typical frontend style)
  (web-mode-markup-indent-offset 2)    ; HTML / tags
  (web-mode-css-indent-offset    2)    ; <style> blocks
  (web-mode-code-indent-offset   2)    ; embedded JS / template code
  (web-mode-script-padding       2)    ; left padding inside <script>
  (web-mode-style-padding        2)    ; left padding inside <style>
  (web-mode-block-padding        2)    ; padding around template control blocks
  ;; Auto-editing (core ergonomics)
  (web-mode-enable-auto-closing     t) ; close tags / brackets
  (web-mode-enable-auto-pairing     t) ; match delimiters
  (web-mode-enable-auto-quoting     t) ; quote attributes automatically
  (web-mode-enable-auto-opening     t) ; expand paired tags on split-line
  (web-mode-enable-auto-indentation t)
  ;; Engine detection: web-mode's core feature -- detect the template engine
  ;; (django, erb, jsp, php, mustache, ...) from file content so the right
  ;; block delimiters and fontification apply.  Override per file type with
  ;; `web-mode-engines-alist'.
  (web-mode-enable-engine-detection t))

;; HTML / web LSP wiring (gated on `web-mode').  Framework resolvers on
;; `my/frontend-web-contacts' take precedence over the default HTML server.
(when (memq 'web-mode my/install-prog-modes)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(((html-mode    :language-id "html")
                    (html-ts-mode :language-id "html")
                    (web-mode     :language-id "html"))
                   . my/web-ls-contact))))

;;; --- Angular (@angular/language-server, ngserver) ---
;; For Angular projects: the TS contact uses ngserver (template
;; type-checking), and the web contact merges ngserver + vscode-html +
;; vscode-css servers via `rass' for component templates.  Registered as
;; resolvers so they only take effect in Angular projects.
;; Install: npm install -g @angular/language-server @angular/language-service
(when (memq 'angular my/install-prog-modes)
  (defun my/angular-project-p ()
    "Return non-nil if current project is an Angular project."
    (when-let* ((project (project-current))
                (root (expand-file-name (project-root project))))
      (or (file-exists-p (expand-file-name "angular.json" root))
          (file-exists-p (expand-file-name "project.json" root)))))

  (defun my/angular-probes ()
    "Return comma-separated probe paths for `ngserver'."
    (let* ((root (expand-file-name (project-root (project-current))))
           (local-nm (expand-file-name "node_modules" root))
           (global-nm (string-trim (shell-command-to-string "npm root -g")))
           (probes (delq nil
                         (list (when (file-directory-p local-nm) local-nm)
                               (unless (string-empty-p global-nm) global-nm)))))
      (mapconcat #'identity probes ",")))

  (defun my/angular-ls-command ()
    "Return command for `ngserver' with probe locations."
    (list "ngserver" "--stdio"
          "--tsProbeLocations" (my/angular-probes)
          "--ngProbeLocations" (my/angular-probes)))

  (defun my/angular--ts-resolver ()
    "ngserver contact for Angular projects, else nil."
    (when (my/angular-project-p) (my/angular-ls-command)))

  (defun my/angular--web-resolver ()
    "rass-merged HTML contact for Angular projects, else nil.

Angular projects merge ngserver + vscode-html-language-server +
vscode-css-language-server via `rass'."
    (when (my/angular-project-p)
      (list "rass" "--"
            "ngserver" "--stdio"
            "--tsProbeLocations" (my/angular-probes)
            "--ngProbeLocations" (my/angular-probes)
            "--" "vscode-html-language-server" "--stdio"
            "--" "vscode-css-language-server" "--stdio")))

  (add-hook 'my/frontend-ts-contacts  #'my/angular--ts-resolver)
  (add-hook 'my/frontend-web-contacts #'my/angular--web-resolver))

;;; --- Vue (Volar, @vue/language-server) ---
;; Vue 3 single-file components via `vue-mode' (MELPA) + Volar.  Decoupled
;; from web-mode so `vue' works even when `web-mode' is not enabled.
;; Install: npm install -g @vue/language-server
(use-package vue-mode
  :ensure t
  :if (memq 'vue my/install-prog-modes)
  :mode "\\.vue\\'"
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((vue-mode :language-id "vue")
                   . ("vue-language-server" "--stdio")))))

(provide 'init-lang-web)

;;; init-lang-web.el ends here
