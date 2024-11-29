local M = {}

M.null_ls_list = {
  -- C/C++ (called by clangd, DON'T configure with null-ls)
  "clang-format",
  -- "clang-tidy",
  -- cmake
  "cmakelang",
  "cmakelint",
  -- git
  "gitlint",
  -- golang
  -- "golangci-lint",
  -- "gofumpt",
  -- "goimports",
  -- "gomodifytags",
  -- "impl",
  -- "revive",
  -- lua
  "stylua",
  -- javascript, typescript, vue, javascriptreact, typescriptreact
  "eslint_d",
  -- python
  -- "black",
  -- "isort",
  "ruff",
  -- "javascript", "javascriptreact", "typescript", "typescriptreact", "vue", "css", "scss", "less", "html", "json", "yaml", "markdown", "graphql"
  "prettier",
  -- shells
  "shfmt",
  "shellcheck", -- TODO: check whether bashls will automatically invoke it
  "beautysh",
  -- markdown
  "marksman",
}

M.lsp_list = {
  "jsonls", -- json
  -- "clangd", -- C/C++
  "neocmake", -- cmake
  -- "gopls", -- golang
  "basedpyright", -- python
  "rust_analyzer", -- rust
  "bashls", -- bash
  "lua_ls", -- lua
  -- "tsserver", -- typescript, javascript
  "ts_ls",
  "vimls", -- vimscript
  "yamlls", -- yaml
  "taplo", -- toml
}

M.dap_list = {
  "python", -- python
  "delve", -- go
  "cpptools", -- cpp
}

return M
