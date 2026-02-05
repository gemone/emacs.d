# Configuration Guide

This repository contains a modular Emacs configuration with support for local customization.

## File Structure

### Main Configuration Files

- **`init.el`** - Main Emacs configuration file
- **`options.el`** - Default variable definitions using `defcustom` (tracked by git)
- **`.gitignore`** - Git ignore rules (excludes local files)

### Local Configuration Files (Not Tracked)

- **`options.local.el`** - Override default values with your settings
- **`early-init.local.el`** - Additional early initialization (loaded after options)
- **`init.local.el`** - Additional personal customizations (loaded after init.el)

These local files are **not tracked by git** and allow you to have machine-specific or personal settings without affecting the main configuration.

---

## Quick Start: Enabling Minuet AI

### Step 1: Create/Edit `options.local.el`

Create `options.local.el` in your `.emacs.d` directory and add:

```elisp
;; Enable Minuet AI
(setq gemo/minuet-enabled t
      gemo/minuet-provider 'openai-fim-compatible
      gemo/minuet-model "deepseek-chat"
      gemo/minuet-api-key "DEEPSEEK_API_KEY"
      gemo/minuet-endpoint "https://api.deepseek.com/beta/completions")
```

### Step 2: Set API Key in Environment

Set the API key as an environment variable:

```bash
# On macOS/Linux (add to ~/.zshrc or ~/.bashrc)
export DEEPSEEK_API_KEY="sk-xxxxx"

# On Windows (set in System Environment Variables)
setx DEEPSEEK_API_KEY "sk-xxxxx"
```

### Step 3: Restart Emacs

Restart Emacs to load the configuration.

---

## File Descriptions

### `options.el` (Tracked by Git)

**Purpose:** Define default configuration variables using `defcustom`.

**Usage:** This file is tracked by git and contains all customizable variables with their default values and documentation. You typically don't edit this file directly.

**Structure:**
```elisp
(defcustom gemo/minuet-enabled nil
  "Whether to enable Minuet AI code completion."
  :type 'boolean
  :group 'gemo)

(defcustom gemo/minuet-provider 'openai-fim-compatible
  "LLM provider for Minuet AI."
  :type '(choice ...)
  :group 'gemo)
```

**Benefits:**
- Variables are discoverable via `M-x customize`
- Each variable has documentation
- Type checking and validation
- Can be customized through the customize interface

---

### `options.local.el` (Not Tracked)

**Purpose:** Override default values with your personal settings.

**Usage:** This file is NOT tracked by git. Create it and add your customizations using `setq`.

**Example:**
```elisp
;; Enable Minuet AI with DeepSeek
(setq gemo/minuet-enabled t
      gemo/minuet-provider 'openai-fim-compatible
      gemo/minuet-model "deepseek-chat"
      gemo/minuet-api-key "DEEPSEEK_API_KEY"
      gemo/minuet-endpoint "https://api.deepseek.com/beta/completions"
      gemo/minuet-n-completions 1
      gemo/minuet-context-window 512)
```

**Notes:**
- This file is loaded after `options.el`, so your `setq` will override the defaults
- You only need to specify the variables you want to change
- Do NOT put actual API keys here - use environment variable names

---

### `early-init.local.el` (Not Tracked)

**Purpose:** Additional early initialization (loaded after options files).

**Usage:** This file is optional. Use it for early initialization that needs to run before the main config loads, after options are defined.

**Example:**
```elisp
;; Early initialization examples
(setq some-early-variable t)
```

---

### `init.local.el` (Not Tracked)

**Purpose:** Personal customizations loaded after main configuration.

**Usage:** Use this file for:
- Custom keybindings
- Personal functions
- Additional packages
- Machine-specific settings

**Example:**
```elisp
;; Custom keybindings
(global-set-key (kbd "C-c j") 'my-favorite-command)

;; Additional packages
(use-package some-package
  :config
  (some-package-mode))

;; Machine-specific settings
(setq cnfonts-personal-fontnames
   '(("YourFont" "YourChineseFont")))
```

---

## Configuration Workflow

### Loading Order

1. **`options.el`** → Default variable definitions
2. **`options.local.el`** → Your custom overrides (if exists)
3. **`early-init.local.el`** → Additional early init (if exists)
4. **`init.el`** → Main configuration (reads variables from options)
5. **`init.local.el`** → Personal customizations (if exists)

### How Variables Work

1. `options.el` defines defaults using `defcustom`
2. `options.local.el` overrides them using `setq` (if file exists)
3. `init.el` reads the final values to configure packages

### Example: Minuet AI Configuration

**In `options.el` (default):**
```elisp
(defcustom gemo/minuet-enabled nil
  "Whether to enable Minuet AI."
  :type 'boolean
  :group 'gemo)
```

**In `options.local.el` (your override):**
```elisp
(setq gemo/minuet-enabled t)  ; Override default
```

**In `init.el` (usage):**
```elisp
(use-package minuet
  :ensure (when (bound-and-true-p gemo/minuet-enabled) t)
  ...)
```

---

## Minuet AI Configuration

### Available Providers

| Provider | Description | Best Models |
|----------|-------------|-------------|
| `openai` | OpenAI GPT models | `gpt-4o-mini`, `gpt-4o` |
| `claude` | Anthropic Claude | `claude-haiku-4-5` |
| `gemini` | Google Gemini | `gemini-2.0-flash` |
| `openai-compatible` | OpenAI-compatible chat APIs | Various |
| `openai-fim-compatible` | OpenAI-compatible completion APIs | `deepseek-chat` |
| `codestral` | Mistral Codestral | `codestral-latest` |

### Configuration Variables

All variables are defined in `options.el` with defaults:

| Variable | Default | Description |
|----------|---------|-------------|
| `gemo/minuet-enabled` | `nil` | Enable Minuet AI when non-nil |
| `gemo/minuet-provider` | `'openai-fim-compatible` | LLM provider to use |
| `gemo/minuet-model` | `"deepseek-chat"` | Model name |
| `gemo/minuet-api-key` | `"DEEPSEEK_API_KEY"` | Env var containing API key |
| `gemo/minuet-endpoint` | `"https://..."` | Custom API endpoint |
| `gemo/minuet-n-completions` | `3` | Number of completions (1 for local, 3 for cloud) |
| `gemo/minuet-context-window` | `16000` | Context size in chars |
| `gemo/minuet-request-timeout` | `3` | Request timeout in seconds |
| `gemo/minuet-debounce-delay` | `0.4` | Delay after typing stops |
| `gemo/minuet-throttle-delay` | `1.0` | Minimum time between requests |
| `gemo/minuet-max-tokens` | `256` | Max tokens to generate |

You can customize these via:
1. **`options.local.el`** - Recommended for most users
2. **`M-x customize-group RET gemo RET`** - Interactive customization UI

---

## Recommended Configurations

### Free & Fast (Cloud)

**Gemini Flash 2.0:**

In `options.local.el`:
```elisp
(setq gemo/minuet-enabled t
      gemo/minuet-provider 'gemini
      gemo/minuet-model "gemini-2.0-flash"
      gemo/minuet-api-key "GEMINI_API_KEY"
      gemo/minuet-endpoint nil)  ; use default
```

Set environment variable:
```bash
export GEMINI_API_KEY="your-key-here"
```

**DeepSeek (Very affordable):**

In `options.local.el`:
```elisp
(setq gemo/minuet-enabled t
      gemo/minuet-provider 'openai-fim-compatible
      gemo/minuet-model "deepseek-chat"
      gemo/minuet-api-key "DEEPSEEK_API_KEY"
      gemo/minuet-endpoint "https://api.deepseek.com/beta/completions")
```

Set environment variable:
```bash
export DEEPSEEK_API_KEY="sk-xxxxx"
```

### High Quality (Paid)

**OpenAI GPT-4o-mini:**

In `options.local.el`:
```elisp
(setq gemo/minuet-enabled t
      gemo/minuet-provider 'openai
      gemo/minuet-model "gpt-4o-mini"
      gemo/minuet-api-key "OPENAI_API_KEY")
```

**Claude Haiku:**

In `options.local.el`:
```elisp
(setq gemo/minuet-enabled t
      gemo/minuet-provider 'claude
      gemo/minuet-model "claude-haiku-4-5"
      gemo/minuet-api-key "ANTHROPIC_API_KEY")
```

### Local LLMs (via Ollama)

First, install Ollama and pull a model:
```bash
ollama pull qwen2.5-coder:3b
```

In `options.local.el`:
```elisp
(setq gemo/minuet-enabled t
      gemo/minuet-provider 'openai-fim-compatible
      gemo/minuet-model "qwen2.5-coder:3b"
      gemo/minuet-api-key "TERM"  ; placeholder for Ollama
      gemo/minuet-endpoint "http://localhost:11434/v1/completions"
      gemo/minuet-n-completions 1      ; single completion for performance
      gemo/minuet-context-window 512)  ; smaller context for speed
```

**Note:** For local LLMs, ensure your model supports FIM (Fill-In-the-Middle) completion. Models like `qwen2.5-coder` and `deepseek-coder-v2` support this, while `deepseek-coder` does not.

---

## Minuet AI Usage

### Keybindings

| Key | Action |
|-----|--------|
| `M-y` | Show completions in minibuffer |
| `M-i` | Show inline suggestion (overlay) |
| `M-n` | Next suggestion |
| `M-p` | Previous suggestion |
| `M-a` | Accept single line |
| `M-A` | Accept entire completion |
| `M-e` | Dismiss suggestion |
| `C-c m` | Configure provider interactively |

### Modes

- **Auto-suggestion mode**: Automatically shows suggestions when you stop typing
  - Enabled automatically in `prog-mode` when `gemo/minuet-enabled` is `t`
  - Can be toggled with `M-x minuet-auto-suggestion-mode`

- **Manual completion**: Use `M-y` or `M-i` to request completions on demand

---

## Troubleshooting

### Minuet Not Loading

1. Check that `options.local.el` exists and sets `gemo/minuet-enabled` to `t`
2. Verify the file is loaded: `C-h v gemo/minuet-enabled`
3. Check `*minuet*` buffer for errors (`C-x b *minuet*`)

### API Key Errors

1. Verify environment variable is set: `M-x getenv RET DEEPSEEK_API_KEY`
2. Ensure variable name in `gemo/minuet-api-key` matches environment variable
3. Do NOT put actual API key in config file - use environment variable name
4. Restart Emacs after setting environment variable

### Slow Completions

For **local LLMs**, add to `options.local.el`:
```elisp
(setq gemo/minuet-context-window 512)    ; Reduce context
      gemo/minuet-n-completions 1)       ; Single completion
      gemo/minuet-request-timeout 10)    ; Increase timeout
```

For **cloud APIs** with rate limits:
```elisp
(setq gemo/minuet-debounce-delay 0.6)    ; Increase debounce
      gemo/minuet-throttle-delay 1.5)    ; Increase throttle
```

### Timeout Errors

Check the `*minuet*` buffer for specific error messages. Common causes:
- Context window too large (reduce to 512 for local LLMs)
- Request timeout too short (increase to 5-10 seconds)
- Model too slow for your hardware (try smaller model)

### Ollama Connection Issues

1. Verify Ollama is running: `curl http://localhost:11434/api/tags`
2. Check model supports FIM: `ollama show qwen2.5-coder:3b`
3. Ensure endpoint URL is correct:
   - FIM: `http://localhost:11434/v1/completions`
   - Chat: `http://localhost:11434/v1/chat/completions`

---

## Advanced Configuration

### Using the Customize Interface

You can also configure variables using Emacs's built-in customization interface:

1. `M-x customize-group RET gemo RET`
2. Modify values interactively
3. Click "Apply and Save"

This will create or update `options.local.el` automatically.

### Custom Provider Examples

**OpenRouter (access to many models):**

In `options.local.el`:
```elisp
(setq gemo/minuet-provider 'openai-compatible
      gemo/minuet-model "mistralai/devstral-small"
      gemo/minuet-api-key "OPENROUTER_API_KEY"
      gemo/minuet-endpoint "https://openrouter.ai/api/v1/chat/completions")
```

**Fireworks AI:**

In `options.local.el`:
```elisp
(setq gemo/minuet-provider 'openai-compatible
      gemo/minuet-model "accounts/fireworks/models/llama-v3p3-70b-instruct"
      gemo/minuet-api-key "FIREWORKS_API_KEY"
      gemo/minuet-endpoint "https://api.fireworks.ai/inference/v1/chat/completions")
```

### Performance Tuning

**For maximum speed (cloud APIs):**

In `options.local.el`:
```elisp
(setq gemo/minuet-n-completions 3          ; Get multiple options
      gemo/minuet-debounce-delay 0.3       ; Faster response
      gemo/minuet-throttle-delay 0.8)      ; Less throttling
```

**For minimum cost (cloud APIs):**

In `options.local.el`:
```elisp
(setq gemo/minuet-n-completions 1          ; Single completion
      gemo/minuet-debounce-delay 0.8       ; Slower response
      gemo/minuet-throttle-delay 2.0       ; More throttling
      gemo/minuet-max-tokens 128)          ; Shorter completions
```

**For best quality:**

In `options.local.el`:
```elisp
(setq gemo/minuet-n-completions 3
      gemo/minuet-context-window 32000     ; More context
      gemo/minuet-max-tokens 512)          ; Longer completions
```

---

## File Summary

| File | Tracked | Purpose | Edit? |
|------|---------|---------|-------|
| `options.el` | ✅ | Define default variables | No (use customize) |
| `options.local.el` | ❌ | Override defaults | Yes (your settings) |
| `early-init.local.el` | ❌ | Additional early init | Optional |
| `init.local.el` | ❌ | Personal customizations | Yes (your settings) |

---

## Resources

- [Minuet AI Documentation](https://github.com/milanglacier/minuet-ai.el)
- [Ollama Models](https://ollama.com/library) - Local LLM models
- [OpenRouter](https://openrouter.ai/) - Compare model speeds and pricing
- [Google AI Studio](https://makersuite.google.com/app/apikey) - Get Gemini API key
- [OpenAI Platform](https://platform.openai.com/api-keys) - Get OpenAI API key
