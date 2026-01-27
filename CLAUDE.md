# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a **dotfiles repository** for Linux (Debian Sid/Ubuntu 24.04) and Windows. Files prefixed with `_` are symlinked to `$HOME/.filename` via `install.py`.

## Common Commands

### Dotfiles Setup

```bash
# Dry run to preview changes
./install.py -n

# Install symlinks (close browsers/other apps first)
./install.py

# Restore original files
./install.py -r
```

### System Package Installation

```bash
# Cargo packages
cd misc/pkgs && cargo install $(cat cargo)

# Python packages
pip install -U -r requirements.txt

# Linux apt packages
sudo apt install $(cat Linux/debs)
```

### Neovim Configuration

```bash
# Basic syntax check
nvim --headless +qa

# Test LSP attachment
nvim --headless +"e test.cpp" +"lua print(vim.inspect(vim.lsp.get_clients()))" +qa

# Profile startup time
nvim --startuptime profile.log
```

### Linting

```bash
# Check Lua formatting
stylua -g '!nvim/lazy' -f _config/nvim/stylua.toml --check .
```

---

## Git Commit Conventions

**ALL commit messages MUST be in English.**

### Format

```
<type>: <description>

<footer (optional)>
```

### Types
- `feat:` - New feature
- `fix:` - Bug fix
- `refactor:` - Code refactoring
- `docs:` - Documentation only
- `style:` - Code style changes
- `test:` - Adding/updating tests
- `chore:` - Maintenance tasks
- `perf:` - Performance improvements

### Co-Authored-By

```
Co-Authored-By: Name <email>
```

---

## Neovim Configuration

### Editor Philosophy
- **Primary tools**: fzf-lua (fuzzy finder), lazy.nvim (plugin manager)
- **Minimal dependencies**: Avoid redundant plugins
- **Performance first**: Use lazy loading where possible
- **Native over external**: Prefer Neovim 0.11+ built-in features

### Code Style
- **No global variables** - Use module-local variables instead of `_G.*`
- **Use `local`** for all module-level variables
- **Clean dead code** - Remove commented-out code promptly

### Architecture

```
_config/nvim/
├── init.lua              # Entry point, lazy.nvim setup
├── lua/
│   ├── core/             # Core settings (options, keymaps, autocmds)
│   ├── plug/             # Plugin configurations
│   │   ├── lsp/          # LSP setup (clangd, gopls, etc.)
│   │   ├── fzf/          # fzf-lua customization
│   │   └── *.lua         # Per-plugin configs
│   └── status.lua        # Statusline
```

### Plugin Loading Strategy
- **LSP servers**: Use `vim.lsp.enable()` in nvim-lspconfig's config function
- **Filetype-specific**: Use `ft = { "c", "cpp" }` for language tools
- **Command-only**: Use `cmd = "CommandName"` for utilities
- **Default**: Prefer `lazy = true` over immediate loading

---

## Language Tooling

### C/C++
- **LSP**: `clangd-19`
- **Formatter**: `clang-format`
- **Root markers**: `compile_commands.json`, `.clangd`, `.git`

### Python
- **LSP**: `basedpyright`
- **Linter/Formatter**: `ruff`

---

## Troubleshooting

### LSP Not Attaching
1. Check root markers exist in project directory
2. Verify `vim.lsp.enable()` includes the server name
3. Check `:LspInfo` for attachment status

### Plugin Loading Issues
1. Check lazy.nvim status with `:Lazy`
2. Use `<leader>ll` (LazyPlugins command) to browse plugins
3. Check for `enabled = false` in plugin specs

### Performance Issues
1. Profile startup: `nvim --startuptime profile.log`
2. Check for non-lazy-loaded plugins
3. Defer heavy operations to `VimEnter` or `VeryLazy`
