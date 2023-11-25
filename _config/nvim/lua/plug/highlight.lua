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
        style = "darker",
      }
    end,
  },
}
