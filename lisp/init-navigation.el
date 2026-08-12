;;; init-navigation.el --- ibuffer and isearch -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer management (ibuffer as the `C-x C-b' replacement) and
;; incremental-search ergonomics (isearch: match counter, immediate
;; highlighting, scroll/wrap behavior).

;;; Code:
;;; IBuffer & isearch
;; ibuffer is the built-in Dired-like buffer manager (Emacs 30.2).

;; isearch-related keys (all built-in defaults):
;;   C-s / C-r   : incremental search over the buffer list
;;   M-s a C-s   : incremental search in marked buffers (ibuffer-do-isearch)
;;   M-s a C-M-s : same, regexp variant (ibuffer-do-isearch-regexp)
;;   Filtering (/ prefix): / n name  / b basename  / f filename  / m mode  / c content
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)   ; use ibuffer instead of the default list-buffers
         :map ibuffer-mode-map
         ;; Already bound by default; declared here for clarity and easy override
         ("M-s a C-s" . ibuffer-do-isearch)
         ("M-s a C-M-s" . ibuffer-do-isearch-regexp))
  :hook ((ibuffer-mode . hl-line-mode)
         (ibuffer-mode . ibuffer-auto-mode))
  :custom
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-show-empty-filter-groups nil))

;;; Isearch
;; Built-in isearch tuning: match counter, immediate multi-match
;; highlighting, and scroll/wrap ergonomics.
(use-package isearch
  :ensure nil
  :custom
  ;; Show "N/total" while searching
  (isearch-lazy-count t)
  ;; Highlight all matches immediately (no idle delay)
  (lazy-highlight-initial-delay 0)
  ;; Allow C-v/M-v and other motion commands during isearch
  (isearch-allow-scroll t)
  ;; Wrap to the other end without pausing
  (isearch-wrap-pause 'no)
  ;; Reversing direction keeps the current search string
  (isearch-repeat-on-direction-change t)
  ;; Whitespace in the pattern matches any run of whitespace
  (search-whitespace-regexp "\\s-+")
  :config
  ;; C-o lists all matches in an Occur buffer
  (define-key isearch-mode-map (kbd "C-o") #'isearch-occur))

(provide 'init-navigation)

;;; init-navigation.el ends here
