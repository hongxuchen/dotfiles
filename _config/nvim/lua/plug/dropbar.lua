return {
  {
    "Bekaboo/dropbar.nvim",
    config = function()
      local dropbar_api = require("dropbar.api")
      vim.keymap.set("n", "gs", dropbar_api.pick, { desc = "[dropbar] Pick symbols in winbar" })
      vim.keymap.set("n", "[;", dropbar_api.goto_context_start, { desc = "[dropbar] Go to start of current context" })
      vim.keymap.set("n", "];", dropbar_api.select_next_context, { desc = "[dropbar] Select next context" })
    end,
    opts = {
      sources = {
        treesitter = false,
        path = {
          modified = function(sym)
            return sym:merge {
              name = sym.name .. " [+]",
              name_hl = "DiffAdded",
            }
          end,
        },
      },
    },
  },
}
