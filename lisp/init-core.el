;;; init-core.el --- startup, dirs, base emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Core runtime concerns that everything else builds on:
;;
;;   - Garbage collection: raise the threshold during minibuffer use and
;;     reclaim the startup garbage on first idle.
;;   - Performance: use-package load statistics, bigger subprocess chunks,
;;     startup-time reporting.
;;   - Base Emacs configuration: shared runtime directories (all under
;;     `~/.emacs.d/var/', gitignored), UI chrome, backups/auto-saves,
;;     tree-sitter/eln cache placement and auto-revert.
;;
;; This module builds on `init-const' (shared runtime dirs: `my/var-dir',
;; `my/cache-dir', `my/state-dir'); other modules may reference them too,
;; so `init-core' is loaded right after `init-package'.  Loaded from
;; `init.el' as `init-core'.

;;; Code:

;;; Garbage collection: fast startup, responsive completion
;; `early-init.el' defers GC during startup by raising
;; `gc-cons-threshold' / `gc-cons-percentage'.  This section restores
;; normal values once startup is over, reclaims the startup garbage during
;; the first idle moment, and keeps GC out of the minibuffer so vertico /
;; orderless / consult completion stays snappy.

(defvar my/gc-normal-threshold (* 128 1024 1024)
  "GC cons threshold (bytes) used during normal editing.")
(defvar my/gc-minibuffer-threshold (* 512 1024 1024)
  "GC cons threshold (bytes) held while the minibuffer is active.")

(defun my/gc-finish-startup ()
  "Restore normal GC settings after startup and schedule a cleanup GC.
`early-init.el' set a large startup threshold; this resets
`gc-cons-threshold' and `gc-cons-percentage' to sane runtime values and
runs one `garbage-collect' after a short idle delay so init-time consing
is reclaimed without blocking input."
  (setq gc-cons-threshold my/gc-normal-threshold)
  (setq gc-cons-percentage 0.1)
  (run-with-idle-timer 5 nil #'garbage-collect))

(add-hook 'emacs-startup-hook #'my/gc-finish-startup)

(defun my/gc-minibuffer-enter ()
  "Raise the GC threshold while the minibuffer is active.
Completion (vertico/orderless/consult) conses heavily while typing; a
large threshold here avoids mid-typing GC pauses."
  (setq gc-cons-threshold my/gc-minibuffer-threshold))

(defun my/gc-minibuffer-exit ()
  "Restore the normal GC threshold when the minibuffer closes."
  (setq gc-cons-threshold my/gc-normal-threshold))

(add-hook 'minibuffer-setup-hook #'my/gc-minibuffer-enter)
(add-hook 'minibuffer-exit-hook #'my/gc-minibuffer-exit)

;;; Performance tuning
;; Track per-package load times; inspect with `M-x use-package-report'.
(setq use-package-compute-statistics t)

;; Native-compile the config modules (lisp/init-*.el) asynchronously, once.
;; `require' then loads the cached .eln on later startups, trimming init
;; time.  The .eln filename embeds a hash of the source, so an edited .el
;; never shadows the .eln — the file simply stops matching.  Only compile
;; when native compilation actually works and the .eln is missing: an
;; Emacs built with comp support but without libgccjit (common on
;; Windows) would otherwise raise "Cannot find libgccjit library" on
;; every idle timer, and `native-compile-async' recompiles unconditionally.
(let* ((lisp-dir (expand-file-name "lisp/" user-emacs-directory))
       (probe (expand-file-name "init-core.el" lisp-dir))
       (native-comp (and (require 'comp nil t)
                         (fboundp 'native-comp-available-p)
                         (native-comp-available-p)))
       (eln (and native-comp (comp-el-to-eln-filename probe))))
  (when (and eln (not (file-exists-p eln)))
    (run-with-idle-timer 5 nil
      (lambda () (native-compile-async lisp-dir 1)))))

;; Subprocess output is read in chunks of this size; the 64 KiB default
;; throttles chatty processes (eglot, magit, ...), so raise it.
(setq read-process-output-max (* 4 1024 1024))

;; Visual order follows buffer order strictly: fine for Chinese/English
;; text and saves some redisplay work.  Disable if you edit RTL scripts
;; (Arabic/Hebrew).
(setq bidi-display-reordering nil)

;; Report startup time so tuning changes are measurable.
(defun my/report-startup-time ()
  "Message total and init-phase startup time."
  (message "Startup finished in %.2fs (init %.2fs)"
           (float-time (time-subtract (current-time) before-init-time))
           (float-time (time-subtract after-init-time before-init-time))))

(add-hook 'emacs-startup-hook #'my/report-startup-time)

(require 'init-const)

;;; Basic emacs config
(use-package emacs :ensure nil
  :preface
  ;; The shared dirs (`my/var-dir' etc.) come from `init-const' (required
  ;; above); this block only makes sure they exist and extends the list
  ;; with derived subdirectories.
  (dolist (dir (list my/cache-dir my/state-dir my/backup-dir my/auto-save-dir
                     (expand-file-name "auto-save-list/" my/cache-dir)
                     (expand-file-name "eshell/" my/state-dir)
                     (expand-file-name "transient/" my/state-dir)
                     (expand-file-name "tree-sitter/" my/cache-dir)
                     (expand-file-name "eln-cache/" my/cache-dir)))
    (make-directory dir t))
  (require 'treesit nil t)
  :custom
  (ring-bell-function #'ignore)
  (initial-frame-alist '((fullscreen . maximized)))
  (inhibit-startup-screen t)
  
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Backups (foo.el~) -> cache/backup/
  (backup-directory-alist `(("." . ,my/backup-dir)))

  ;; Auto-saves (#foo.el#) -> cache/auto-save/, still recoverable via
  ;; `recover-file' (it derives the name the same way).
  ;;
  ;; Use `concat' + `file-name-as-directory', NOT `expand-file-name':
  ;; (expand-file-name "\\1" DIR) treats "\\1" as an absolute path on
  ;; Windows (backslash is a separator there) and silently drops DIR,
  ;; collapsing the replacement to the drive root ("c:/1"), so the
  ;; autosave file ends up under "C:\\#..." and the write fails -- no
  ;; autosave/recover file is ever produced.  `concat' keeps DIR and
  ;; preserves the "\\1" backref on every platform; on POSIX the result
  ;; is identical to the old `expand-file-name' form.
  (auto-save-file-name-transforms
   `((".*" ,(concat (file-name-as-directory my/auto-save-dir) "\\1") t)))
  (auto-save-list-file-prefix
   (expand-file-name "auto-save-list/.saves-" my/cache-dir))

  ;; eshell history is state, not cache
  (eshell-directory-name (expand-file-name "eshell/" my/state-dir))

  ;; Native-compiled elisp files are a pure cache
  (native-comp-eln-load-path
   (cons (expand-file-name "eln-cache/" my/cache-dir)
         (cdr native-comp-eln-load-path)))

  ;; Tree-sitter grammars are compiled caches
  (treesit-extra-load-path
   (if (boundp 'treesit-extra-load-path)
       (cons (expand-file-name "tree-sitter/" my/cache-dir)
             treesit-extra-load-path)))

  ;; Auto-revert buffers when the file on disk changes
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)

  :config
  (set-frame-parameter nil 'alpha-background 95)

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (context-menu-mode t)

  (show-paren-mode t)

  (global-auto-revert-mode 1)

  ;; Tramp connection history lives in var/state.  It's a defcustom, so a
  ;; plain `setq' in :custom would be reset when tramp loads; set it here
  ;; instead.  (Declared for byte-compile: tramp loads lazily.)
  (defvar tramp-persistency-file-name)
  (with-eval-after-load 'tramp
    (setq tramp-persistency-file-name (expand-file-name "tramp" my/state-dir)))

)

(provide 'init-core)

;;; init-core.el ends here
