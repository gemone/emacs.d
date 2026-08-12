;;; init-appearance.el --- theme, fonts, table alignment -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual appearance: the Catppuccin theme, fontaine font presets (with
;; CJK fallback probing) and valign for pixel-perfect table alignment.
;; The font probing needs a real graphical frame, so daemon startup
;; defers it until the first client frame appears.

;;; Code:
;;; theme
(use-package catppuccin-theme
  :ensure t
  ;; Load and apply immediately at startup so the theme is already active
  ;; when the first frame is shown (no flash / pause under the default
  ;; theme).
  :demand t
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (catppuccin-reload))

;;; font config
;; Load via :ensure + :demand only when a GUI is available (normal startup)
;; or when running as a daemon, which may get a graphical client frame later.
;; Plain terminal sessions never load fontaine at all.
(use-package fontaine
  :ensure t
  :demand t
  :if (or (display-graphic-p) (daemonp))
  :config
  ;; Probe installed fonts at startup and pick the first available family
  ;; from each chain, so the config works on machines with only some of the
  ;; fonts installed.  Maple Mono ships as SEPARATE families: NF has Nerd
  ;; Font icons (PUA), CN has CJK glyphs; some builds also provide a
  ;; combined "NF CN".  Prefer the combined family when present, then NF
  ;; (icons for mode-line/completion), then CN / generic CJK-capable
  ;; fallbacks.  The chosen latin family is used for the fontaine presets,
  ;; and `face-font-family-alternatives' keeps the whole chain as backup.
  (defun my/font-available-p (family)
    "Return non-nil if FAMILY is installed on the current display."
    (member family (font-family-list)))

  (defun my/select-font (families)
    "Return the first installed font in FAMILIES.
If no font can be probed (e.g. daemon/terminal at load time) or none is
installed, return the first element of FAMILIES as a safe default."
    (catch 'found
      (dolist (family families)
        (when (my/font-available-p family)
          (throw 'found family)))
      (car families)))

  ;; Font probing and fontsets need a real graphical frame, so all the
  ;; fontaine setup is collected here and only run when a GUI is present.
  (defun my/fontaine-apply ()
    "Probe fonts, define presets, and apply the regular preset.
Only meaningful on a graphical display; terminal frames use their own
face settings and have no need for fontaine."
    (let* ((latin-chain '("Maple Mono NF CN" "Maple Mono NF" "Maple Mono CN"
                          "CaskaydiaCove Nerd Font Mono" "Cascadia Code"
                          "JetBrains Mono Nerd Font Mono" "Iosevka Nerd Font Mono"
                          "DejaVu Sans Mono" "Monospace"))
           (cjk-chain '("Maple Mono NF CN" "Maple Mono CN" "LXGW WenKai"
                        "Sarasa Mono SC" "WenQuanYi Micro Hei Mono"))
           (main-font (my/select-font latin-chain))
           (cjk-font  (my/select-font cjk-chain)))
      (setq face-font-family-alternatives (list latin-chain))
      ;; CJK primary + remaining installed candidates as glyph-level fallback.
      ;; font-spec + explicit "fontset-default" is reliable on pgtk; NAME=t
      ;; with a bare family string often silently fails there.
      (set-fontset-font "fontset-default" 'han (font-spec :family cjk-font))
      (dolist (family (cdr (member cjk-font cjk-chain)))
        (set-fontset-font "fontset-default" 'han (font-spec :family family) nil 'append))
      (setq fontaine-presets
            `((regular :default-height 130)
              (large   :default-height 160)
              (t       :default-family ,main-font
                       :default-weight regular
                       :fixed-pitch-family ,main-font
                       :variable-pitch-family ,main-font
                       :bold-weight semibold
                       :italic-slant italic
                       :line-spacing nil))))
    ;; fontaine-mode only persists the last preset across restarts; it does
    ;; NOT apply fonts. fontaine-set-preset is what actually sets faces.
    (fontaine-mode 1)
    (fontaine-set-preset 'regular))

  (when (display-graphic-p)
    (my/fontaine-apply)))

;; Daemon start has no frame yet, so fontaine cannot run at init time.
;; Apply it once the first graphical client frame appears (emacsclient -c);
;; the new frame is selected before this hook runs.  Terminal client frames
;; (emacsclient -t) are skipped by the display-graphic-p check.
(defun my/fontaine-apply-on-gui-frame ()
  "Apply fontaine settings when a graphical client frame appears."
  (when (display-graphic-p)
    (my/fontaine-apply)))
(when (daemonp)
  (add-hook 'server-after-make-frame-hook #'my/fontaine-apply-on-gui-frame))

;; Pixel-perfect vertical alignment for variable-pitch/CJK columns.
(use-package valign
  :ensure t
  ;; valign only does anything in org buffers (pixel-perfect table alignment,
  ;; incl. CJK 2x-width glyphs); the old prog-mode hook was a no-op.
  :hook (org-mode . valign-mode)
  :config
  ;; Maple Mono NF CN / CJK fallbacks are true 2x-width, so valign's default
  ;; width table already matches; no need to add custom entry.
  (setq valign-fancy-bar nil))

(provide 'init-appearance)

;;; init-appearance.el ends here
