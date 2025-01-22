return {
  "ibhagwan/fzf-lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {},
  config = function()
    require("plug.fzf.conf")
    require("plug.fzf.customize")
    require("plug.fzf.providers.locate")
  end,
}
