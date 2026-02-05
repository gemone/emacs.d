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

## File Descriptions

### `options.el` (Tracked by Git)

**Purpose:** Define default configuration variables using `defcustom`.

**Usage:** This file is tracked by git and contains all customizable variables with their default values and documentation. You typically don't edit this file directly.

**Structure:**
```elisp
(defcustom gemo/my-variable nil
  "Documentation for this variable."
  :type 'boolean
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
;; Example: Override default variables
(setq gemo/my-variable t
      another-variable "custom-value")
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

### Example: Variable Configuration

**In `options.el` (default):**
```elisp
(defcustom gemo/my-feature-enabled nil
  "Whether to enable my feature."
  :type 'boolean
  :group 'gemo)
```

**In `options.local.el` (your override):**
```elisp
(setq gemo/my-feature-enabled t)  ; Override default
```

**In `init.el` (usage):**
```elisp
(use-package my-package
  :if (bound-and-true-p gemo/my-feature-enabled)
  :config
  (my-package-mode))
```

---

## Using the Customize Interface

You can configure variables using Emacs's built-in customization interface:

1. `M-x customize-group RET gemo RET`
2. Modify values interactively
3. Click "Apply and Save"

This will create or update `options.local.el` automatically.

---

## Common Lisp Development (SLIME)

This configuration includes SLIME (Superior Lisp Interaction Mode for Emacs) for Common Lisp development.

### Installation

1. **Install a Lisp implementation**

   On macOS:
   ```bash
   brew install sbcl
   ```

   On Linux:
   ```bash
   sudo apt-get install sbcl  # Debian/Ubuntu
   sudo yum install sbcl      # Fedora/RHEL
   ```

   On Windows:
   - Download SBCL from http://www.sbcl.org/platform-table.html
   - Or use WSL on Windows

2. **Restart Emacs** - SLIME will be automatically installed via elpaca

### Usage

**Start SLIME:**
- `SPC l i` - Start SLIME with default Lisp implementation
- `M-x slime` - Start SLIME interactively

**Keybindings:**
- `SPC l s` - SLIME selector (switch between REPL, buffers, etc.)
- `SPC l r` - Reset connection
- `SPC l c` - Interrupt running process
- `SPC l q` - Quit SLIME
- `C-c C-z` - Switch to/from SLIME REPL

**SLIME REPL Commands:**
- `,` followed by command name
- `,quit` - Quit SLIME
- `,help` - Show help
- `,reload` - Reload current file

### Lisp Implementations

The default implementation is SBCL (Steel Bank Common Lisp). To change it:

In `options.local.el`:
```elisp
(setq inferior-lisp-program "clisp")  ; or "ecl", "abcl", etc.
```

Available implementations:
- **sbcl** - Steel Bank Common Lisp (recommended, default)
- **ccl** - Clozure Common Lisp (fast compilation)
- **clisp** - GNU CLISP (widely available)
- **ecl** - Embedded Common Lisp
- **abcl** - Armed Bear Common Lisp (JVM-based)

### Features

- **Fuzzy completion** - Intelligent code completion
- **Interactive debugging** - Full-featured debugger
- **REPL integration** - Interactive development
- **Compilation feedback** - Real-time compilation notes
- **Cross-reference** - Find callers/callees of functions
- **Inspector** - Inspect Lisp objects

### More Information

- [SLIME Official Site](https://slime.common-lisp.dev/)
- [SLIME Manual](https://slime.common-lisp.dev/doc/html/)
- [SBCL Manual](http://www.sbcl.org/manual/)

---

## File Summary

| File | Tracked | Purpose | Edit? |
|------|---------|---------|-------|
| `options.el` | ✅ | Define default variables | No (use customize) |
| `options.local.el` | ❌ | Override defaults | Yes (your settings) |
| `early-init.local.el` | ❌ | Additional early init | Optional |
| `init.local.el` | ❌ | Personal customizations | Yes (your settings) |

---

## Troubleshooting

### SLIME Not Starting

1. Check that SBCL is installed: `which sbcl`
2. If SBCL is installed but Emacs can't find it:
   - Check Emacs PATH: `M-x getenv RET PATH`
   - Make sure Homebrew prefix is in PATH: `/opt/homebrew/bin`
3. Try starting SLIME manually: `M-x slime`

### SLIME Connection Issues

1. Verify SBCL is accessible from Emacs:
   ```elisp
   M-: (executable-find "sbcl")
   ```
2. Check `*inferior-lisp*` buffer for error messages
3. Try restarting the REPL: `SPC l r`

### Package Installation Issues

If packages fail to install:
1. Check `elpaca` directory exists
2. Review `*elpaca*` buffer for errors
3. Try: `M-x elpaca-process-queues`

---

## Resources

### Emacs
- [Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Emacs Wiki](https://www.emacswiki.org/)

### Packages
- [Elpaca](https://github.com/progfolio/elpaca) - Package manager
- [Evil](https://github.com/emacs-evil/evil) - Vim emulation
- [Vertico](https://github.com/minad/vertico) - Completion UI
- [Corfu](https://github.com/minad/corfu) - In-buffer completion

### Common Lisp
- [SLIME Official Site](https://slime.common-lisp.dev/)
- [SLIME Manual](https://slime.common-lisp.dev/doc/html/)
- [SBCL Manual](http://www.sbcl.org/manual/)
- [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/)
