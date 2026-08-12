;;; init-lang-web-angular.el --- Angular (@angular/language-server) -*- lexical-binding: t; -*-

;;; Commentary:
;; Angular projects: the TypeScript contact uses ngserver (template
;; type-checking), and the web contact merges ngserver + vscode-html +
;; vscode-css servers via `rass' for component templates.  Registered as
;; resolvers on the `my/frontend-*-contacts' hooks consulted by the
;; dispatchers in `init-lang-web-basic', so they only take effect in
;; Angular projects.  Loaded only when `web-angular' is in
;; `my/install-prog-modes' — `init.el' gates this module.
;; Install: npm install -g @angular/language-server @angular/language-service

;;; Code:
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
(add-hook 'my/frontend-web-contacts #'my/angular--web-resolver)

(provide 'init-lang-web-angular)

;;; init-lang-web-angular.el ends here
