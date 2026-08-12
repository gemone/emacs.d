;;; init-completion.el --- vertico/orderless/consult/corfu/cape -*- lexical-binding: t; -*-

;;; Commentary:
;; Minibuffer and in-buffer completion stack: vertico (minibuffer UI),
;; orderless (matching), consult (search/navigation commands),
;; consult-dir + zoxide (directory jumping), corfu (in-buffer popup) and
;; cape (extra completion-at-point sources layered under eglot).
;; `savehist' keeps minibuffer history in `my/state-dir'.

;;; Code:
;;; Completion
;; MiniBuff
(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  :init
  (vertico-mode))

(use-package savehist
  :ensure nil
  :custom
  ;; Minibuffer history is persistent state, not cache
  (savehist-file (expand-file-name "history" my/state-dir))
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; Note: `+orderless-consult-dispatch' was removed in current Consult;
  ;; only `orderless-affix-dispatch' exists (`!' exclude, `=' literal).
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  ;; LSP candidates (eglot) are already prefix-filtered by the server, so
  ;; use `basic' matching for them; orderless would otherwise re-filter and
  ;; hide valid candidates.
  (completion-category-overrides '((eglot-capf (styles basic))
                                   (file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t))

(defun my/consult-find ()
  "Fuzzy-find files with `fd' when available, else fall back to `find'."
  (interactive)
  (call-interactively (if (executable-find "fd") #'consult-fd #'consult-find)))

(use-package consult
  :ensure t
  :bind (("C-c f" . my/consult-find)
         ("C-c F" . consult-ripgrep)
         ("C-s" . consult-line))
  :config
  ;; Use projectile roots instead of the built-in project.el.
  ;; (`consult-project-function' takes MAY-PROMPT; ignore it.)
  ;; projectile is loaded lazily by an idle timer (init-project), so force
  ;; `require' here: consult commands run the moment the user invokes them,
  ;; possibly before projectile has loaded — calling `projectile-project-root'
  ;; directly would signal "void-function".
  (setq consult-project-function
        (lambda (&rest _)
          (require 'projectile)
          (projectile-project-root))))

(use-package zoxide
  :ensure t)

(use-package consult-dir
  :ensure t
  ;; Note: C-c d is taken by dape's key prefix, so use C-c z (zoxide).
  :bind (("C-c z" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-j" . consult-dir-jump-file))
  :config
  ;; zoxide history as a consult-dir source; narrow with `z'.
  (defvar consult-dir-source-zoxide
    `(:name "Zoxide"
            :narrow ?z
            :category file
            :face consult-file
            :history file-name-history
            :enabled ,(lambda () (featurep 'zoxide))
            :items ,#'zoxide-query)
    "Zoxide directory source for `consult-dir'.")
  (add-to-list 'consult-dir-sources 'consult-dir-source-zoxide t))

;; For Code
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;; Quit (hide popup) when there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  (corfu-auto t)                 ;; Popup while typing (idle-based)
  (corfu-auto-delay 0.2)         ;; Idle delay before showing candidates
  (corfu-auto-prefix 2)          ;; Minimum prefix length for auto popup

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-mouse-mode)
  (corfu-popupinfo-mode))

;; Extra completion sources layered on top of LSP (eglot):
;; - eglot registers its capf buffer-locally, so it is tried FIRST and
;;   provides server completions for identifiers;
;; - when eglot returns nil (comments, strings, no server, non-LSP modes)
;;   these global sources fill in: file names, words in the buffer,
;;   dictionary words, and elisp code blocks.
(use-package cape
  :ensure t
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(provide 'init-completion)

;;; init-completion.el ends here
