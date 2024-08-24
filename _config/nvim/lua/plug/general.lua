return {

  { "tridactyl/vim-tridactyl" },

  { "nvim-neotest/nvim-nio" },

  {
    "tibabit/vim-templates",
    config = function()
      local uv = vim.uv
      vim.g.tmpl_search_paths = { vim.fn.stdpath("config") .. "templates" }
      local hostname = uv.os_gethostname()
      if vim.startswith(hostname, "hw") then
        vim.g.tmpl_author_name = vim.env.HW_NAME
        vim.g.tmpl_author_email = vim.env.EMAIL
      else
        vim.g.tmpl_author_email = vim.env.EMAIL
      end
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
    -- config = function()
    --   require("indent_blankline").setup {
    --   }
    -- end,
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    config = true,
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
