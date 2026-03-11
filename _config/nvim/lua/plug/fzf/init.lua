return {
  "ibhagwan/fzf-lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = true,
  cmd = {
    "DocumentSymbols",
    "Env",
    "Flocate",
    "FzfLua",
    "FzfLspInfo",
    "LazyPlugins",
    "ListFilesFromBranch",
  },
  init = function()
    require("plug.fzf.keymaps")
  end,
  opts = {},
  config = function()
    require("plug.fzf.conf")
    require("plug.fzf.customize")
    require("plug.fzf.providers.locate")
  end,
}
