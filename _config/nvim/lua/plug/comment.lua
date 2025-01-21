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
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("todo-comments").setup {}
    end,
    keys = {
      { "<leader>td", "<cmd>TodoFzfLua<cr>", desc = "[TODO] show todo comments" },
      {
        "]t",
        function()
          require("todo-comments").jump_next()
        end,
        desc = "[TODO] next todo comment",
      },
      {
        "[t",
        function()
          require("todo-comments").jump_prev()
        end,
        desc = "[TODO] prev todo comment",
      },
    },
  },
}
