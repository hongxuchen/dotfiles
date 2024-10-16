return {
  {
    "hongxuchen/semantic-highlight.nvim",
    config = function()
      require("local-highlight").setup {
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
  {
    "Verf/deepwhite.nvim",
    lazy = true,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("deepwhite")
    end,
  },
}
