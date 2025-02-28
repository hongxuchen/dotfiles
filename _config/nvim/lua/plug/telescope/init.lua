return {
  {
    "nvim-telescope/telescope.nvim",
    event = "VeryLazy",
    dependencies = {
      { "nvim-telescope/telescope-symbols.nvim" },
      { "tsakirist/telescope-lazy.nvim" },
    },
    config = function()
      local u = require("core.utils")
      local t = require("telescope")
      local actions = require("telescope.actions")

      t.setup {
        defaults = {
          layout_strategy = "horizontal",
          sorting_strategy = "ascending",
          layout_config = {
            horizontal = {
              prompt_position = "top",
            },
          },
          preview = {
            check_mime_type = true,
            filesize_limit = require("core.utils").SizeInMegaBytes,
            timeout = 100,
            treesitter = false,
            msg_bg_fillchar = "/",
            hide_on_startup = false,
          },
          color_devicons = false,
          history = false,
          wrap_results = false,
          prompt_prefix = " ",
          selection_caret = " ",
          path_display = { "truncate" },
          mappings = {
            i = {
              ["PageUp"] = false,
              ["PageDown"] = false,
              ["<M-n>"] = actions.results_scrolling_down,
              ["<M-p>"] = actions.results_scrolling_up,
            },
          },
        },
        symbols = { "emoji", "gitmoji", "math", "latex" },
      }

      t.load_extension("lazy") -- lazy plugin navigation

      u.keymap("n", "<leader>ll", "<Cmd>Telescope lazy<CR>", u.opts, "[telescope] navigate to lazy plugins")
      u.keymap("n", "<leader>ss", "<Cmd>Telescope symbols<CR>", u.opts, "[telescope] complete symbols")
    end,
  },
}
