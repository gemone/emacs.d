;;; init-dired.el --- dired file manager -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired (the built-in file manager) ergonomics:
;;
;;   - Listing: GNU ls with human-readable sizes, directories first.
;;   - Hygiene: DWIM copy/move targets (other window), recursive ops
;;     without confirmation, one dired buffer reused, buffers auto-revert
;;     when the filesystem changes, missing target dirs auto-created.
;;   - Safety: deletions go to the trash instead of `rm'ing forever.
;;   - Navigation: hide dotfiles/temp files (dired-x omit), dired-subtree
;;     tree view, dired-narrow live filtering, wdired rename-by-editing,
;;     dired-open for external apps.
;;
;; Meow keeps dired buffers in MOTION state automatically (j/k move, all
;; the dired action keys unchanged), so nothing extra is needed here.

;;; Code:

;; Built-in dired tuning.  Emacs 30 options (`dired-make-directory-clickable',
;; `dired-mouse-drag-files', `dired-create-destination-dirs',
;; `dired-auto-revert-*') used below are only referenced if bound.
(use-package dired
  :ensure nil
  :demand t
  :custom
  ;; GNU ls: human-readable sizes, directories first
  (dired-listing-switches "-lah --group-directories-first")
  ;; Copy/move target defaults to the other window (or previous dir)
  (dired-dwim-target t)
  ;; Never ask before recursive copy/delete
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  ;; Delete moves to trash instead of permanent rm
  (delete-by-moving-to-trash t)
  ;; Replace the dired buffer when opening a directory (no buffer bloat)
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Refresh the listing when files change underneath us
  (dired-auto-revert-on-buffer-change t)
  (dired-auto-revert-on-directory-change t)
  ;; Preserve file mtime on copy; auto-create missing target dirs
  (dired-copy-preserve-time t)
  ;; Emacs 30: clickable directory names / drag & drop between frames
  (dired-make-directory-clickable t)
  (dired-mouse-drag-files t)
  ;; Isearch only over file names by default
  (dired-isearch-filenames 'dwim)
  :bind
  (:map dired-mode-map
        ;; `-': go up to the parent directory (ranger convention); frees
        ;; the default negative-argument, which is useless in dired
        ("-" . dired-up-directory))
  :config
  (when (boundp 'dired-create-destination-dirs)
    (setq dired-create-destination-dirs 'always)))

;; dired-x: `dired-omit-mode' hides dotfiles & Emacs temp/lock files.
;; It also provides `dired-jump' (C-x C-j) — quick jump to the directory
;; of the current buffer.
(use-package dired-x
  :ensure nil
  :after dired
  :demand t
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  :config
  ;; Extend the default ".hidden" pattern with "#file#"/"#file#" temp files.
  ;; Set here (not :custom): :custom evaluates while `dired-omit-files' is
  ;; still being declared, so referencing it would be a void-variable error.
  (setq dired-omit-files (concat dired-omit-files "\\|^#.*#$")))

;; wdired: rename / move / symlink files by editing the listing directly.
;; Enter with `C-x C-q', commit with `C-c C-c'.
(use-package wdired
  :ensure nil
  :after dired
  :custom
  (wdired-allow-move t)
  (wdired-create-parent-directories t))

;; dired-subtree: expand/collapse subdirectories inline as a tree.
;; This version ships no default keys, so bind them explicitly:
;;   i / TAB  : toggle the subtree under point
;; (`"TAB"' not `"<tab>"': in Emacs 30 (kbd "<tab>") yields the [tab]
;; function key, which no longer matches the literal TAB character that
;; dired-mode-map uses.)
(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
         ("i" . dired-subtree-toggle)
         ("TAB" . dired-subtree-toggle)))

;; dired-narrow: live-filter the listing to matching file names.
;; Default binding: `/'.
(use-package dired-narrow
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow-fuzzy)))

;; dired-open: `e' opens the file under point with the system app
;; (xdg-open on Linux), falling back to Emacs for known modes.
(use-package dired-open
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("e" . dired-open)))

;; diff-hl: git status markers in Dired, plus in-file fringe indicators
;; while editing (`diff-hl-mode' in prog-mode).  In Dired each changed
;; file gets a fringe bitmap + the status letter (M/S/C...); `=' diffs
;; the file under point.
(use-package diff-hl
  :ensure t
  :after dired
  :hook ((dired-mode . diff-hl-dired-mode)
         (prog-mode . diff-hl-mode)))

;; dired-git-info: `;' toggles git branch + last commit info in the
;; Dired header (like `git log -n 1 --oneline' for this directory).
(use-package dired-git-info
  :ensure t
  :after dired
  :bind (:map dired-mode-map (";" . dired-git-info-mode)))

;;; `-' = back to the file manager, everywhere
;; In Dired `-' already moves to the parent directory (bound above).  In
;; any other buffer, meow NORMAL `-' is bound to this command in
;; init-editing; here we also cover MOTION state (read-only/special
;; buffers).  INSERT state keeps `-' as a literal dash, so typing is
;; unaffected.
(defun my/dired-jump-or-parent ()
  "Jump to the current file's directory, or up a level in Dired.
In a Dired buffer, move to the parent directory.  Otherwise open the
directory of the current buffer's file in Dired (see `dired-jump')."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (dired-up-directory)
    (dired-jump)))

(with-eval-after-load 'meow
  (meow-motion-define-key '("-" . my/dired-jump-or-parent)))

(declare-function meow-motion-define-key "meow-helpers")

(provide 'init-dired)

;;; init-dired.el ends here
