;;; init-extras.el --- agent-shell and ghostel -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous tools that don't belong to a larger concern:
;; `agent-shell' (LLM coding agent frontend over ACP) and `ghostel' (a
;; fast terminal emulator).

;;; Code:
;; LLM coding agent frontend: a comint shell over ACP (Agent Client
;; Protocol). Supports Claude Code, Codex, Gemini CLI, Pi, Goose, etc.
;; Open via M-x agent-shell (C-u for a new shell).
(use-package agent-shell
  :ensure t
  ;; ~10k lines loaded on first `M-x agent-shell' instead of at startup.
  :defer t
  :commands agent-shell)

;; Ghostel: fast terminal emulator using libghostty-vt.
;; Requires dynamic module support (module-file-suffix non-nil).
;; Native binary auto-downloads on first use. Open via M-x ghostel.
(use-package ghostel
  :ensure t
  :bind ("C-x m" . ghostel))

(provide 'init-extras)

;;; init-extras.el ends here
