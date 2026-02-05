;;; options.el --- Configuration variable definitions -*- lexical-binding: t; -*-

;; This file is tracked by git and contains default variable definitions.
;; Use options.local.el to override these values for your local setup.

;; ============================================
;; Minuet AI Configuration
;; ============================================

(defcustom gemo/minuet-enabled nil
  "Whether to enable Minuet AI code completion.
When non-nil, minuet-ai package will be loaded and configured."
  :type 'boolean
  :group 'gemo)

(defcustom gemo/minuet-provider 'openai-fim-compatible
  "LLM provider for Minuet AI.

Available options:
- 'openai                  : OpenAI GPT models
- 'claude                  : Anthropic Claude models
- 'gemini                  : Google Gemini models
- 'openai-compatible       : OpenAI-compatible chat APIs
- 'openai-fim-compatible   : OpenAI-compatible completion APIs (FIM)
- 'codestral               : Mistral Codestral"
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Claude" claude)
                 (const :tag "Gemini" gemini)
                 (const :tag "OpenAI Compatible" openai-compatible)
                 (const :tag "OpenAI FIM Compatible" openai-fim-compatible)
                 (const :tag "Codestral" codestral))
  :group 'gemo)

(defcustom gemo/minuet-model "deepseek-chat"
  "Model name for Minuet AI.

Examples by provider:
- OpenAI: \"gpt-4o-mini\", \"gpt-4o\", \"gpt-4.1-mini\"
- Claude: \"claude-haiku-4-5\", \"claude-3-5-haiku-20241022\"
- Gemini: \"gemini-2.0-flash\", \"gemini-2.5-flash\"
- OpenAI-compatible: \"deepseek-chat\", \"deepseek-coder\"
- OpenAI-FIM-compatible: \"deepseek-chat\"
- Codestral: \"codestral-latest\"
- Ollama: \"qwen2.5-coder:3b\", \"deepseek-coder-v2\""
  :type 'string
  :group 'gemo)

(defcustom gemo/minuet-api-key "DEEPSEEK_API_KEY"
  "Environment variable name containing the API key.

This should be the name of the environment variable, NOT the actual key value.

Common options:
- \"OPENAI_API_KEY\"       for OpenAI
- \"ANTHROPIC_API_KEY\"   for Claude
- \"GEMINI_API_KEY\"      for Gemini
- \"DEEPSEEK_API_KEY\"    for DeepSeek
- \"CODESTRAL_API_KEY\"   for Codestral
- \"TERM\" or \"APPDATA\" for Ollama (placeholder)"
  :type 'string
  :group 'gemo)

(defcustom gemo/minuet-endpoint "https://api.deepseek.com/beta/completions"
  "Custom API endpoint for OpenAI-compatible providers.
Set to nil to use the provider's default endpoint.

Examples:
- DeepSeek FIM: \"https://api.deepseek.com/beta/completions\"
- DeepSeek Chat: \"https://api.deepseek.com/chat/completions\"
- OpenRouter: \"https://openrouter.ai/api/v1/chat/completions\"
- Ollama FIM: \"http://localhost:11434/v1/completions\"
- Ollama Chat: \"http://localhost:11434/v1/chat/completions\"
- nil: Use provider's default endpoint"
  :type '(choice (string :tag "Custom endpoint")
                 (const :tag "Default endpoint" nil))
  :group 'gemo)

(defcustom gemo/minuet-n-completions 3
  "Number of completion candidates to request.

- 1: Recommended for local LLMs (faster, less resource usage)
- 3: Recommended for cloud APIs (more options)
- Larger values will increase API costs and latency."
  :type 'integer
  :group 'gemo)

(defcustom gemo/minuet-context-window 16000
  "Maximum context characters sent to LLM (before and after cursor).
Smaller values = faster but less context.

Recommended values:
- 512-1024:   For local LLMs
- 16000:      Default for cloud APIs
- 32000+:     For powerful machines with cloud APIs"
  :type 'integer
  :group 'gemo)

(defcustom gemo/minuet-request-timeout 3
  "Maximum timeout in seconds for completion requests.
Increase this if you experience timeout errors.

Recommended values:
- 3:   Default, good for cloud APIs
- 5-10: Recommended for local LLMs
- 10+:  For slow models or connections"
  :type 'integer
  :group 'gemo)

(defcustom gemo/minuet-debounce-delay 0.4
  "Delay in seconds before sending request after typing stops.
Larger values reduce API costs but increase perceived latency.

Recommended values:
- 0.3-0.4:  For fast responses
- 0.6-1.0:  To reduce API costs"
  :type 'number
  :group 'gemo)

(defcustom gemo/minuet-throttle-delay 1.0
  "Minimum time in seconds between completion requests.
Larger values reduce rate limit issues but decrease responsiveness.

Recommended values:
- 0.8-1.0:  For fast responses
- 1.5-2.0:  To avoid rate limits"
  :type 'number
  :group 'gemo)

(defcustom gemo/minuet-max-tokens 256
  "Maximum tokens to generate per completion.
Larger values allow longer completions but increase latency.

Recommended values:
- 56-64:    For code completion (faster)
- 128-256:  Balanced (default)
- 512+:     For longer completions (slower)"
  :type 'integer
  :group 'gemo)

;;; options.el ends here
