return {
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup {
        pre_hook = function(ctx)
          local status_ok, ts_comment = pcall(require, "ts_context_commentstring.integrations.comment_nvim")
          if status_ok then
            return ts_comment.create_pre_hook()(ctx)
          end
          return nil
        end,
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
