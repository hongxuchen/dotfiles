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
  },
  -- Optional dependencies
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = false,
}
