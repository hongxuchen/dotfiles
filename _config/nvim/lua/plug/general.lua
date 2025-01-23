return {

  { "nvim-neotest/nvim-nio" },
  { "nvim-lua/plenary.nvim", lazy = true },

  -- {
  --   "tibabit/vim-templates",
  --   config = function()
  --     local uv = vim.uv
  --     -- should override default paths rather than add
  --     vim.g.tmpl_search_paths = { vim.fn.stdpath("config") .. "templates" }
  --     local hostname = uv.os_gethostname()
  --     if vim.startswith(hostname, "hw") then
  --       vim.g.tmpl_author_name = vim.env.HW_NAME
  --       vim.g.tmpl_author_email = vim.env.EMAIL
  --     else
  --       vim.g.tmpl_author_email = vim.env.EMAIL
  --     end
  --   end,
  -- },

  {
    "MagicDuck/grug-far.nvim",
    config = function()
      require("grug-far").setup {}
    end,
  },

  -- profiling
  { "dstein64/vim-startuptime", cmd = "StartupTime" },

  -- chinese punctuation replacement
  {
    "hotoo/pangu.vim",
    cmd = { "Pangu", "PanguAll" },
    config = function()
      vim.g.pangu_rule_duplicate_punctuation = 0
      vim.g.pangu_rule_spacing = 0
      vim.g.pangu_rule_date = 0
      vim.g.pangu_punctuation_brackets = {}
      vim.g.pangu_punctuation_ellipsis = {}
    end,
  },

  {
    "MeanderingProgrammer/render-markdown.nvim",
    enabled = true,
    dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
    ---@module 'render-markdown'
    ---@type render.md.UserConfig
    opts = {
      enabled = false,
      heading = {
        enabled = false,
      },
      paragraph = {
        enabled = false,
      },
      code = {
        enabled = true,
      },
      bullet = {
        enabled = false,
      },
      checkbox = {
        enabled = true,
      },
      quote = {
        enabled = true,
      },
    },
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>k",
        function()
          require("which-key").show { global = true, ["local"] = true }
        end,
        desc = "[wk] Buffer Local Keymaps",
      },
    },
    config = true,
    opts = {
      preset = "modern",
      expand = 20,
      show_help = false,
      show_keys = false,
      icons = {
        rules = false,
        colors = false,
      },
      sort = { "alphanum", "order", "group", "mod", "local" },
    },
  },

  {
    "moll/vim-bbye",
    keys = {
      { "<leader>qq", "<Cmd>Bdelete<CR>", desc = "delete buffer nicely" },
    },
  },

  { "kyazdani42/nvim-web-devicons", lazy = true },
  {
    "mortepau/codicons.nvim",
    config = true,
  },

  -- unix operation utilities(auto configured)
  { "tpope/vim-eunuch" },
}
