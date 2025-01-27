return {
  "stevearc/oil.nvim",
  ---@module 'oil'
  ---@type oil.SetupOpts
  opts = {
    default_file_explorer = true,
    watch_for_changes = true,
    skip_confirm_for_simple_edits = true,
    view_options = {
      show_hidden = true,
      case_insensitive = true,
      sort = {
        { "name", "asc" },
        { "type", "asc" },
      },
    },
    git = {
      -- Return true to automatically git add/mv/rm files
      add = function(path)
        return true
      end,
      mv = function(src_path, dest_path)
        return true
      end,
      rm = function(path)
        return false
      end,
    },
  },
  keys = {
    {
      "-",
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
