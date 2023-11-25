return {
  {
    "rhysd/git-messenger.vim",
    cmd = "GitMessenger",
    config = function()
      vim.g.git_messenger_no_default_mappings = true
    end,
  },
  -- fugitive does not need to setup
  { "tpope/vim-fugitive" },
  { "junegunn/gv.vim"},
}
