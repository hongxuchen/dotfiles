return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      bash = { "beautysh", "shfmt", "shellharden" },
      c = { "clang-format" },
      cpp = { "clang-format" },
      css = { "prettierd", "prettier", stop_after_first = true },
      html = { "prettierd", "prettier", stop_after_first = true },
      javascript = { "prettierd", "prettier", stop_after_first = true },
      javascriptreact = { "prettierd", "prettier", stop_after_first = true },
      json = { "prettierd", "prettier", stop_after_first = true },
      jsonc = { "prettierd", "prettier", stop_after_first = true },
      less = { "prettierd", "prettier", stop_after_first = true },
      lua = { "stylua" },
      python = { "ruff_format", "ruff_fix", "ruff_organize_imports" }, -- ruff lsp is better
      rust = { "rustfmt", lsp_format = "fallback" },
      scss = { "prettierd", "prettier", stop_after_first = true },
      sh = { "beautysh", "shfmt" },
      typescript = { "prettierd", "prettier", stop_after_first = true },
      typescriptreact = { "prettierd", "prettier", stop_after_first = true },
      vue = { "prettierd", "prettier", stop_after_first = true },
      yaml = { "prettierd", "prettier", stop_after_first = true },
      zsh = { "beautysh", "shfmt" },
    },
    format_on_save = {
      lsp_format = "fallback",
      timeout_ms = 500,
    },
  },
}
