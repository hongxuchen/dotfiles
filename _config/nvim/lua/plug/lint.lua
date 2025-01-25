return {
  "mfussenegger/nvim-lint",
  config = function(_, opts)
    require("lint").linters_by_ft = {
      c = {}, -- linted by clangd
      cpp = {}, -- linted by clangd
      python = {}, -- linted by ruff
      gitcommit = {},
      sh = { "shellcheck" }, -- shellcheck used by bashls
      bash = {}, -- shellcheck used by bashls
      zsh = { "zsh" }, -- shellcheck used by bashls
      sql = { "sqruff" },
    }

    vim.api.nvim_create_autocmd({
      -- "BufReadPost", -- not working for gitcommit
      -- "BufWritePost",
      "InsertLeave",
    }, {
      desc = "Lint",
      callback = function()
        require("lint").try_lint()
      end,
    })
  end,
}
