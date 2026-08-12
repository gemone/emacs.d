;;; init-lang-web-vue.el --- Vue (vue-mode + Volar) -*- lexical-binding: t; -*-

;;; Commentary:
;; Vue 3 single-file components via `vue-mode' (MELPA) + Volar
;; (@vue/language-server).  Loaded only when `web-vue' is in
;; `my/install-prog-modes' — `init.el' gates this module.
;; Install: npm install -g @vue/language-server

;;; Code:
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((vue-mode :language-id "vue")
                   . ("vue-language-server" "--stdio")))))

(provide 'init-lang-web-vue)

;;; init-lang-web-vue.el ends here
