;;; init-lang-java.el --- Java: jdtls + eglot-java + dape + java-server -*- lexical-binding: t; -*-

;;; Commentary:
;; Full Java development environment: `eglot-java' (jdtls integration),
;; `dape' (DAP debugger) and LuciusChen's `java-server' toolkit
;; (multi-JDK switching, Tomcat deploy, Spring Boot run/stop, HCR).
;; Gated on `java' being in `my/install-prog-modes' — `init.el' loads this
;; module only when it is enabled.

;;; Code:
;; Resolve the shared dirs (`my/cache-dir' etc.) at compile time too, so
;; byte/native-compilation sees them as bound (no "free variable" noise).
(eval-when-compile (require 'init-const))

;;; --- Java: full development environment (jdtls + eglot-java + dape + java-server) ---
;; References:
;;   - https://emacs-china.org/t/emacs-eglot-eglot-java-dape-java/30086
;;   - https://github.com/LuciusChen/java-server
;;
;; Components:
;;   - eglot-java: eglot extension for JDTLS. Downloads eclipse.jdt.ls on the
;;     first open of a .java file (upgrade manually with
;;     `M-x eglot-java-upgrade-lsp-server'), and provides project/class
;;     creation, Maven/Gradle build tasks, and JUnit running (C-u prefix
;;     enters debug mode, JPDA port 8000).
;;   - dape: DAP client. The built-in `jdtls' config launches the main class;
;;     the `jdtls-jpda' config below attaches to the JPDA port of
;;     JUnit/external Tomcat (forum approach: dape -> java-debug adapter
;;     -> target JVM).
;;   - java-server: LuciusChen's toolkit. Multi-JDK switching, external
;;     Tomcat deploy/stop, Spring Boot run/stop, hot code replace (HCR).
;;
;; External dependencies:
;;   - JDK 17+ (required by jdtls; switch with java-server-select-jdk
;;     for older projects)
;;   - Maven or Gradle (build; prefer the project's own wrapper)
;;   - java-debug plugin jar (required by dape; run `mvn -DskipTests package'
;;     in the microsoft/java-debug repo; the jar lands under extension/server/)
;;   - External Tomcat only needed for WAR deployment (macOS: brew install tomcat@9)

(use-package eglot-java
  :preface
  (defun my/java-debug-plugin-jar ()
    ;; Return the first java-debug plugin jar found in common locations.
    (let ((roots (list (expand-file-name "java-debug" my/cache-dir)
                       (expand-file-name "java-debug" "~")
                       (expand-file-name "debug-adapters" my/cache-dir)
                       "/tmp/java-debug")))
      (catch 'found
        (dolist (root roots)
          (dolist (sub '("extension/server" "server" ""))
            (let ((dir (expand-file-name sub root)))
              (when (file-directory-p dir)
                (dolist (file (directory-files
                               dir t "^com\\.microsoft\\.java\\.debug\\.plugin-.*\\.jar$"))
                  (throw 'found file)))))))))
  (defun my/eglot-java-init-options (_server _jdt)
    ;; JDTLS initialization options: load the java-debug bundle for dape.
    (when-let* ((jar (my/java-debug-plugin-jar)))
      `(:bundles [,jar])))
  :ensure t
  :after eglot
  :hook ((java-mode java-ts-mode) . eglot-java-mode)
  :custom
  ;; eglot-java rewrites `eglot-server-programs' by default; keep manual
  ;; control so java-mode and java-ts-mode share the same jdtls entry.
  (eglot-java-eglot-server-programs-manual-updates t)
  ;; Keep jdtls workspace metadata in the cache dir (the old `-data')
  (eglot-java-eclipse-jdt-cache-directory
   (expand-file-name "jdtls-workspace" my/cache-dir))
  :config
  ;; Load the java-debug bundle so dape can spawn the adapter via JDTLS
  (setq eglot-java-user-init-opts-fn #'my/eglot-java-init-options)
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . eglot-java--eclipse-contact))
  :bind (:map eglot-java-mode-map
         ("C-c j n" . eglot-java-file-new)
         ("C-c j N" . eglot-java-project-new)
         ("C-c j t" . eglot-java-run-test)          ; C-u = debug (JPDA :8000)
         ("C-c j T" . eglot-java-project-build-task)
         ("C-c j R" . eglot-java-project-build-refresh)
         ("C-c j m" . eglot-java-run-main)          ; C-u = debug
         ("C-c j u" . eglot-java-upgrade-lsp-server)
         ("C-c j U" . eglot-java-upgrade-junit-jar)))

(use-package dape
  :preface
  (defun my/dape-jdtls-jpda-ensure (_config)
    ;; Ensure the current buffer has a JDTLS server with java-debug.
    (let ((server (and (featurep 'eglot) (eglot-current-server))))
      (unless server
        (user-error "No active JDTLS (eglot) server in buffer %s" (current-buffer)))
      (unless (seq-contains-p
               (ignore-errors (eglot--server-capable :executeCommandProvider :commands))
               "vscode.java.startDebugSession")
        (user-error "JDTLS does not have the java-debug plugin loaded; cannot start the debug adapter")))
    t)
  (defun my/dape-jdtls-jpda-fn (config)
    ;; Point dape at a java-debug adapter spawned by the current JDTLS.
    ;; The target JVM's JPDA port comes from `:jpda-port' (default 8000,
    ;; matching the debug port used by `eglot-java-run-test').
    (let* ((server (eglot-current-server))
           (adapter-port (eglot-execute-command
                          server "vscode.java.startDebugSession" nil)))
      (thread-first config
        (plist-put 'host "localhost")
        (plist-put 'port adapter-port)
        (plist-put :type "java")
        (plist-put :request "attach")
        (plist-put :hostName "localhost")
        (plist-put :port (or (plist-get config :jpda-port) 8000))
        (plist-put :projectName (project-name (project-current t))))))
  :ensure t
  ;; Load on first `M-x dape' (~240 ms saved at startup).
  :defer t
  :commands dape
  :custom
  ;; Use `kbd' (not a raw string): dape calls `global-set-key' with this
  ;; value at load time, and a plain "C-c d" string would be read as the
  ;; literal characters C - c SPC d instead of a key sequence.
  (dape-key-prefix (kbd "C-c d"))
  (dape-buffer-window-arrangement 'right)
  :config
  (repeat-mode +1)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  ;; JPDA attach for JUnit / external Tomcat (forum approach):
  ;; dape -> java-debug adapter (spawned by JDTLS) -> target JVM (JPDA port)
  (add-to-list 'dape-configs
               '(jdtls-jpda
                 modes (java-mode java-ts-mode)
                 ensure my/dape-jdtls-jpda-ensure
                 fn my/dape-jdtls-jpda-fn
                 :request "attach"
                 :type "java"
                 :hostName "localhost"
                 :jpda-port 8000
                 :projectName nil)))

;; java-server: LuciusChen's Java server development toolkit
;; (multi-JDK switching, Tomcat deploy, Spring Boot run/stop, HCR)
(use-package java-server
  :ensure (:host github :repo "LuciusChen/java-server")
  :after (eglot dape)
  :hook ((java-mode java-ts-mode) . java-server-mode)
  :custom
  ;; Keep generated artifacts in the cache dir (var/); don't clutter ~/.emacs.d
  (java-server-debug-adapters-dir (expand-file-name "debug-adapters" my/cache-dir))
  (java-server-tomcat-instances-dir (expand-file-name "java-server/tomcat" my/cache-dir))
  (java-server-direct-attach-hcr-helper-dir (expand-file-name "java-server/hcr" my/cache-dir))
  :bind (:map java-server-mode-map
         ("C-c J j" . java-server-select-jdk)
         ("C-c J a" . java-server-auto-select-jdk)
         ("C-c J t" . java-server-tomcat-deploy)    ; C-u = JPDA debug
         ("C-c J T" . java-server-tomcat-stop)
         ("C-c J s" . java-server-spring-boot-run)  ; C-u = JPDA debug
         ("C-c J S" . java-server-spring-boot-stop)
         ("C-c J h" . java-server-hot-replace)
         ("C-c J d" . dape)))

(provide 'init-lang-java)

;;; init-lang-java.el ends here
