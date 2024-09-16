return {
{
  "andymass/vim-matchup",
  lazy = false,
  config = function()
    -- disable matchparen and matchit
    vim.g.loaded_matchparen = 1
    vim.g.loaded_matchit = 1
    vim.g.matchup_matchparen_offscreen = { method = "popup" }
  end,
},
  {
    "windwp/nvim-autopairs",
    event = { "InsertEnter" },
    config = function()
      local npairs = require("nvim-autopairs")
      npairs.setup {
        check_ts = true,
        map_c_w = true,
        enable_check_bracket_line = false,
      }
    end,
  },
  {
    "kylechui/nvim-surround",
    event = { "InsertEnter" },
    config = true,
  },
}
