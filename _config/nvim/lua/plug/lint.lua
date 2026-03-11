return {
  "mfussenegger/nvim-lint",
  ft = { "sh", "sql", "zsh" },
  config = function(_, opts)
    local u = require("core.utils")
    local lint = require("lint")
    lint.linters_by_ft = {
      c = {}, -- linted by clangd
      cpp = {}, -- linted by clangd
      python = {}, -- linted by ruff
      gitcommit = {},
      sh = { "shellcheck" }, -- shellcheck used by bashls
      bash = {}, -- shellcheck used by bashls
      zsh = { "zsh" }, -- shellcheck used by bashls
      sql = { "sqruff" },
    }

    vim.api.nvim_create_autocmd("BufWritePost", {
      group = u.myAutoGroup,
      desc = "Lint",
      callback = function(args)
        if vim.bo[args.buf].buftype ~= "" or not vim.bo[args.buf].modifiable then
          return
        end
        local linters = lint.linters_by_ft[vim.bo[args.buf].filetype] or {}
        if #linters == 0 then
          return
        end
        vim.api.nvim_buf_call(args.buf, function()
          lint.try_lint()
        end)
      end,
    })
  end,
}
