return {
  {
    "tzachar/local-highlight.nvim",
    config = function()
      require("local-highlight").setup {
        animate = {
          enabled = false,
        },
        -- hlgroup = "CurSearch",
        -- cw_hlgroup = nil,
      }
    end,
  },
  {
    "navarasu/onedark.nvim",
    enabled = true,
    config = function()
      local onedark = require("onedark")
      onedark.setup {
        style = "warmer",
      }
    end,
  },
}
