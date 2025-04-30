return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      { "nvim-treesitter/playground", cmd = "TSPlaygroundToggle" },
      { "nushell/tree-sitter-nu" },
      { "nvim-treesitter/nvim-treesitter-textobjects" },
    },
    -- smart spellcheck requires treesitter, no lazy-load
    lazy = false,
    config = function()
      vim.keymap.set("n", "<localleader>ti", "<Cmd>TSConfigInfo<CR>", { desc = "[treesitter] Display Config" })
      vim.keymap.set("n", "<localleader>tt", ":TSBufToggle ", { desc = "[treesitter] Toggle Feature" })

      local ignored_langs = { "rust", "markdown", "vimdoc", "help" }
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
        -- textobjects plugin
        textobjects = {
          select = {
            enable = true,

            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,

            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["af"] = { query = "@function.outer", desc = "[ts-obj] outer function object" },
              ["if"] = { query = "@function.inner", desc = "[ts-obj] inner function object" },
              ["ac"] = { query = "@class.outer", desc = "[ts-obj] outer class object" },
              ["ic"] = { query = "@class.inner", desc = "[ts-obj] inner class object" },
              ["al"] = { query = "@loop.outer", desc = "[ts-obj] outer loop object" },
              ["il"] = { query = "@loop.inner", desc = "[ts-obj] inner loop object" },
              ["ad"] = { query = "@loop.outer", desc = "[ts-obj] outer condition object" },
              ["id"] = { query = "@loop.inner", desc = "[ts-obj] inner condition object" },
            },
            include_surrounding_whitespace = true,
          },
          swap = {
            enable = true,
            swap_next = {
              ["<localleader>oa"] = "@parameter.inner",
            },
            swap_previous = {
              ["<localleader>oA"] = "@parameter.inner",
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              ["]f"] = "@function.outer",
              ["]c"] = "@class.outer",
              ["]b"] = "@condition.outer",
              ["]l"] = "@loop.outer",
            },
            goto_next_end = {
              ["]F"] = "@function.outer",
              ["]C"] = "@class.outer",
              ["]B"] = "@condition.outer",
              ["]L"] = "@loop.outer",
            },
            goto_previous_start = {
              ["[f"] = "@function.outer",
              ["[c"] = "@class.outer",
              ["[b"] = "@condition.outer",
              ["[l"] = "@loop.outer",
            },
            goto_previous_end = {
              ["[F"] = "@function.outer",
              ["[C"] = "@class.outer",
              ["[B"] = "@condition.outer",
              ["[L"] = "@loop.outer",
            },
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
