return {
  "stevearc/oil.nvim",
  ---@module 'oil'
  ---@type oil.SetupOpts
  opts = {
    default_file_explorer = true,
    watch_for_changes = true,
  },
  -- config = function()
  --   require("oil").setup {}
  -- end,
  keys = {
    {
      "<leader>e",
      "<Cmd>Oil<CR>",
      desc = "[oil] open file explorer",
    },
    {
      "gd",
      function()
        vim.g.oil_detail = not vim.g.oil_detail
        if vim.g.oil_detail then
          require("oil").set_columns { "icon", "permissions", "size", "mtime" }
        else
          require("oil").set_columns { "icon" }
        end
      end,
      desc = "[oil] toggle file detail view",
    },
  },
  -- Optional dependencies
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = false,
}
