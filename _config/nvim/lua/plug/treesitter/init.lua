return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      { "nvim-treesitter/playground", cmd = "TSPlaygroundToggle" },
      { "nushell/tree-sitter-nu" },
    },
    -- smart spellcheck requires treesitter, no lazy-load
    lazy = false,
    config = function()
      vim.keymap.set("n", "<localleader>ti", "<Cmd>TSConfigInfo<CR>", { desc = "[treesitter] Display Config" })
      vim.keymap.set("n", "<localleader>tt", ":TSBufToggle ", { desc = "[treesitter] Toggle Feature" })

      local ignored_langs = { "c", "cpp", "rust", "markdown", "vimdoc", "help" }
      local ts_config = require("nvim-treesitter.configs")
      ---@diagnostic disable-next-line: missing-fields
      ts_config.setup {
        ignore_install = {},
        auto_install = false,
        ensure_installed = {
          "nu",
          "bash",
          "c",
          "cmake",
          "cpp",
          "css",
          "dockerfile",
          "dot",
          "git_config",
          "gitignore",
          "go",
          "gomod",
          "gosum",
          "gowork",
          "graphql",
          "html",
          "ini",
          "json",
          "latex",
          "llvm",
          "lua",
          "luadoc",
          "markdown",
          "mermaid",
          "ninja",
          "python",
          "query",
          "rst",
          "rust",
          "toml",
          "typescript",
          "vim",
          "vimdoc",
          "yaml",
          "javascript",
        },
        sync_install = true,
        indent = {
          enable = false, -- it's buggy so disable it
        },
        -- for vim-matchup
        matchup = {
          enable = true, -- mandatory, false will disable the whole extension
          disable = ignored_langs, -- optional, list of language that will be disabled
          disable_virtual_text = true,
          include_match_words = true,
        },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
          disable = function(lang, buf)
            if vim.list_contains(ignored_langs, lang) then
              return true
            end
            return require("core.utils").is_bigfile(buf)
          end,
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<localleader>ts",
            node_incremental = "<localleader>ti",
            node_decremental = "<localleader>td",
            scope_incremental = "<localleader>tc",
          },
        },
      }
      -- too slow so unset
      -- vim.opt.foldmethod = "expr"
      -- vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
    end,
    build = function()
      require("nvim-treesitter.install").update { with_sync = true }
    end,
  },
  {
    "Badhi/nvim-treesitter-cpp-tools",
    -- Optional: Configuration
    opts = function()
      local options = {
        preview = {
          quit = "q", -- optional keymapping for quit preview
          accept = "<tab>", -- optional keymapping for accept preview
        },
        header_extension = "h", -- optional
        source_extension = "cpp", -- optional
        custom_define_class_function_commands = { -- optional
          TSCppImplWrite = {
            output_handle = require("nt-cpp-tools.output_handlers").get_add_to_cpp(),
          },
          --[[
                <your impl function custom command name> = {
                    output_handle = function (str, context) 
                        -- string contains the class implementation
                        -- do whatever you want to do with it
                    end
                }
                ]]
        },
      }
      return options
    end,
    -- End configuration
    config = true,
  },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    cond = require("core.utils").not_win,
    config = function()
      require("ts_context_commentstring").setup {
        -- disabled with nvim-comment plugin
        enable_autocmd = false,
      }
      vim.g.skip_ts_context_commentstring_module = true
    end,
  },
}
