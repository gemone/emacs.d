;;; font.el --- Font configuration -*- lexical-binding: t; -*-

;;; Font Variables
(defconst my-english-fonts
  '("CaskaydiaCove Nerd Font" "Fira Code" "JetBrains Mono"
    "SF Mono" "Menlo" "Monaco" "Consolas" "Cascadia Code"
    "DejaVu Sans Mono" "Ubuntu Mono" "Liberation Mono"
    "Hack" "Source Code Pro" "Inconsolata")
  "English fonts for coding.")

(defconst my-chinese-fonts
  '("LXGW WenKai Mono"
    "PingFang SC" "PingFang TC" "Hiragino Sans GB"
    "Microsoft YaHei" "SimHei" "SimSun"
    "Noto Sans Mono CJK SC" "Noto Sans CJK SC"
    "WenQuanYi Zen Hei" "WenQuanYi Micro Hei"
    "Source Han Sans CN")
  "Chinese fonts.")

(defconst my-extb-fonts
  '("HanaMinB")
  "EXT-B fonts for rare Chinese characters.")

(defconst my-symbol-fonts
  '("Segoe UI Symbol" "Symbola" "Apple Symbols" "Arial Unicode MS")
  "Symbol fonts.")

(defconst my-ornament-fonts
  '("NanumGothic" "Arial Unicode MS")
  "Ornament fonts for UI decorations.")

(use-package cnfonts
  :ensure t
  :demand t
  :config
  (setq cnfonts-use-system-type t)
  (setq cnfonts-personal-fontnames
        (list
         my-english-fonts
         my-chinese-fonts
         my-extb-fonts
         my-symbol-fonts
         my-ornament-fonts))

  ;; Enable cnfonts mode
  (cnfonts-mode 1)
  ;; Use Emacs state in cnfonts-ui-mode to avoid evil keybinding conflicts
  (with-eval-after-load 'evil
    (add-to-list 'evil-emacs-state-modes 'cnfonts-ui-mode))
  ;; Font size adjustment shortcuts
  (general-def
    :keymaps 'cnfonts-mode-map
    "C--" #'cnfonts-decrease-fontsize
    "C-=" #'cnfonts-increase-fontsize))

(provide 'font)
;;; font.el ends here
