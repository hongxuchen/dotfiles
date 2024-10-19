return {

  { "nvim-neotest/nvim-nio" },

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
      require("grug-far").setup {
        -- options, see Configuration section below
        -- there are no required options atm
        -- engine = 'ripgrep' is default, but 'astgrep' can be specified
      }
    end,
  },

  { "nvim-lua/plenary.nvim", lazy = true },

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
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    cmd = "IBLEnable",
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>k",
        function()
          require("which-key").show { global = false }
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
