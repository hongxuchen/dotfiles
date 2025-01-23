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

  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require("nvim-treesitter.configs").setup {
        ensure_installed = { "nu" }, -- Ensure the "nu" parser is installed
        highlight = {
          enable = true, -- Enable syntax highlighting
        },
        -- OPTIONAL!! These enable ts-specific textobjects.
        -- So you can hit `yaf` to copy the closest function,
        -- `dif` to clear the content of the closest function,
        -- or whatever keys you map to what query.
        textobjects = {
          select = {
            enable = true,
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              -- For example:
              -- Nushell only
              ["aP"] = "@pipeline.outer",
              ["iP"] = "@pipeline.inner",

              -- supported in other languages as well
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["al"] = "@loop.outer",
              ["il"] = "@loop.inner",
              ["aC"] = "@conditional.outer",
              ["iC"] = "@conditional.inner",
              ["iS"] = "@statement.inner",
              ["aS"] = "@statement.outer",
            }, -- keymaps
          }, -- select
        }, -- textobjects
      }
    end,
    dependencies = {
      -- Install official queries and filetype detection
      -- alternatively, see section "Install official queries only"
      { "nushell/tree-sitter-nu" },
    },
    build = ":TSUpdate",
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
