return {
  "LunarVim/bigfile.nvim",
  enabled = true,
  config = function()
    require("bigfile").setup {
      filesize = require("core.utils").SizeInMegaBytes,
      pattern = { "*" },
      features = {
        "lsp",
      },
    }
  end,
}
