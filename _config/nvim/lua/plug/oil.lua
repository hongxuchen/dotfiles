return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = function()
    return {
      default_file_explorer = true,
      watch_for_changes = true,
      skip_confirm_for_simple_edits = false,
      view_options = {
        show_hidden = true,
        case_insensitive = true,
        sort = {
          { "name", "asc" },
          { "type", "asc" },
        },
      },
      git = {
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
      keymaps = {
        ["<leader>gf"] = {
          callback = function()
            local oil = require("oil")
            local prefills = { paths = oil.get_current_dir() }
            local grug_far = require("grug-far")
            if not grug_far.has_instance("explorer") then
              grug_far.open {
                instanceName = "explorer",
                prefills = prefills,
                staticTitle = "Find and Replace from Oil",
              }
            else
              grug_far.open_instance("explorer")
              grug_far.update_instance_prefills("explorer", prefills, false)
            end
          end,
          desc = "[oil] GrugFar Search in directory",
        },
      },
    }
  end,
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
}
