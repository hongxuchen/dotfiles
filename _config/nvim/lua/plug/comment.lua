return {
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup {
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
      }
    end,
  },
  {
    "folke/todo-comments.nvim",
    keys = {
      { "<leader>t", "<cmd>TodoTelescope<cr>", desc = "[TODO] show TODOs" },
    },
    config = function()
      require("todo-comments").setup {}
    end,
  },
}
