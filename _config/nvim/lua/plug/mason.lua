return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    config = function()
      require("mason").setup()
    end,
  },

  {
    "jayp0521/mason-nvim-dap.nvim",
    event = "VeryLazy",
    config = function()
      require("mason-nvim-dap").setup {
        ensure_installed = require("common").dap_list,
      }
    end,
  },

  {
    "jayp0521/mason-null-ls.nvim",
    config = function()
      require("mason-null-ls").setup {
        ensure_installed = require("common").null_ls_list,
      }
    end,
  },

  {
    "williamboman/mason-lspconfig.nvim",
    -- should be set beforhand otherwise LSs will not be recognized
    lazy = false,
    config = function()
      require("mason-lspconfig").setup {
        ensure_installed = require("common").lsp_list,
      }
    end,
  },
}
