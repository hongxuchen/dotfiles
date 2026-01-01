return {
  "stevearc/conform.nvim",
  keys = {
    {
      "<localleader>f",
      function()
        local conform = require("conform")
        local formatter_names = conform.list_formatters_for_buffer(0)

        conform.format {
          async = false,
          lsp_fallback = true,
          quiet = false,
        }

        local msg = #formatter_names > 0 and table.concat(formatter_names, ", ") or "LSP"
        vim.notify(string.format("Formatted with: [%s]", msg), vim.log.levels.INFO)
      end,
      mode = { "n", "v" },
      desc = "[conform] format buffer/selection",
    },
  },
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
    format_on_save = nil,
    -- format_on_save = {
    --   lsp_format = "fallback",
    --   timeout_ms = 500,
    -- },
  },
}
