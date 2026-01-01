# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with this dotfiles repository, specifically the Neovim configuration.

## Git Commit Conventions

**ALL commit messages MUST be in English.**

### Commit Message Format

```
<type>: <description>

<footer (optional)>
```

**Types:**
- `feat:` - New feature
- `fix:` - Bug fix
- `refactor:` - Code refactoring
- `docs:` - Documentation only
- `style:` - Code style changes (formatting, etc.)
- `test:` - Adding or updating tests
- `chore:` - Maintenance tasks
- `perf:` - Performance improvements

**Examples:**
```
feat: add fzf-lua lazy plugins picker

Add :LazyPlugins command to browse and open lazy.nvim plugin directories
```

```
fix: enable LSP servers with vim.lsp.enable()

In Neovim 0.11+, vim.lsp.config() only configures servers.
vim.lsp.enable() is required to actually start them.
```

### Co-Authored-By Footer

DO NOT use "Generated with Claude Code" or similar auto-generated footers.
If multiple authors contributed, use standard Git co-authorship:
```
Co-Authored-By: Name <email>
```

---

## Neovim Configuration Preferences

### Editor Philosophy

- **Primary tools**: fzf-lua (fuzzy finder), lazy.nvim (plugin manager)
- **Minimal dependencies**: Avoid redundant plugins (e.g., removed telescope in favor of fzf-lua)
- **Performance first**: Use lazy loading where possible
- **Native over external**: Prefer Neovim 0.11+ built-in features over external plugins

### Code Style Preferences

1. **No global variables** - Use module-local variables instead of `_G.*`
2. **Consistent naming** - Use `local` for all module-level variables
3. **Clean dead code** - Remove commented-out code promptly
4. **Fix typos** - Correct spelling errors in comments and code

### Configuration Architecture

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
└── ...
```

### Plugin Loading Strategy

- **LSP servers**: Use `vim.lsp.enable()` in nvim-lspconfig's config function
- **Filetype-specific**: Use `ft = { "c", "cpp" }` for language tools
- **Command-only**: Use `cmd = "CommandName"` for utilities
- **Default**: Prefer `lazy = true` over immediate loading

---

## Common Tasks

### Adding a New LSP Server

1. Add to `mason-lspconfig` `ensure_installed` list
2. Configure with `vim.lsp.config()` in `lua/plug/lsp/init.lua`
3. Add to `vim.lsp.enable()` list
4. Test: `nvim --headless +"e test.xxx" +"lua print(vim.lsp.get_clients())" +qa`

### Adding a New fzf-lua Picker

Add to `lua/plug/fzf/customize.lua`:
```lua
mycmd("CommandName", function()
  fzf.fzf_exec(entries, {
    prompt = "Prompt> ",
    actions = {
      ["default"] = function(selected, _)
        -- Handle selection
      end,
    },
  })
end, {})
```

### Testing Configuration

```bash
# Basic syntax check
nvim --headless +qa

# Test LSP attachment
nvim --headless +"e /path/to/file.cpp" +"lua print(vim.inspect(vim.lsp.get_clients()))" +qa

# Check specific Lua code
nvim --headless +"lua print('test')" +qa
```

---

## Project-Specific Notes

### clangd Configuration

- **Binary**: `clangd-19` (system package)
- **Root markers**: `compile_commands.json`, `.clangd`, `.git`
- **Key feature**: Automatically detects project root

### Python Tooling

- **LSP**: `basedpyright` (type checking)
- **Linter**: `ruff` (fast, replaces multiple tools)
- **Formatter**: `ruff` (via conform.nvim)

### C++ Tooling

- **LSP**: `clangd-19`
- **Formatter**: `clang-format`
- **Keybinds**: `<localleader>d*` for clangd-specific commands

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
3. Consider deferring heavy operations to `VimEnter` or `VeryLazy`
