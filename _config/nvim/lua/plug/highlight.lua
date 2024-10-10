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
    enabled = false,
    config = function()
      local onedark = require("onedark")
      onedark.setup {
        style = "warmer",
      }
    end,
  },
  {
    "projekt0n/github-nvim-theme",
    enabled = false,
    name = "github-theme",
    lazy = true,
    priority = 1000,
    config = function()
      require("github-theme").setup {}
      -- vim.cmd("colorscheme github_dark_high_contrast")
    end,
  },
  {
    "Verf/deepwhite.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd("colorscheme deepwhite")
    end,
  },
}
