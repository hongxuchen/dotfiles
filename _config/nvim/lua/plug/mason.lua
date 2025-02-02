return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    config = function()
      require("mason").setup()
    end,
  },

  {
    "zapling/mason-conform.nvim",
    config = function()
      require("mason-conform").setup {}
    end,
  },

  {
    "rshkarin/mason-nvim-lint",
    config = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
      require("mason-nvim-lint").setup {
        ignore_install = {
          "sqruff",
          "zsh",
          "commitlint", -- mason registered but not by this plugin
        },
        automatic_installation = true,
      }
    end,
  },

  {
    "jayp0521/mason-nvim-dap.nvim",
    event = "VeryLazy",
    config = function()
      require("mason-nvim-dap").setup {
        ensure_installed = {},
        automatic_installation = true,
      }
    end,
  },

  {
    "williamboman/mason-lspconfig.nvim",
    -- should be set beforhand otherwise LSs will not be recognized
    lazy = false,
    config = function()
      require("mason-lspconfig").setup {
        -- those used but not configured by lspconfig
        ensure_installed = {
          "ruff", -- python
          "jsonls", -- json
          -- "clangd", -- C/C++ use special clangd
          "neocmake", -- cmake
          -- "gopls", -- golang
          "basedpyright", -- python
          -- "rust_analyzer", -- rust(use rustup one)
          "bashls", -- bash
          "lua_ls", -- lua
          -- "tsserver", -- typescript, javascript
          "ts_ls",
          "vimls", -- vimscript
          "yamlls", -- yaml
          "taplo", -- toml
          "jdtls", -- java
        },
        automatic_installation = { exclude = {} },
      }
    end,
  },
}
