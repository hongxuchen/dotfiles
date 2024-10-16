return {
  {
    "nvimtools/none-ls.nvim",
    config = function()
      local null_ls = require("null-ls")
      local formatting = null_ls.builtins.formatting
      local diagnostics = null_ls.builtins.diagnostics
      local code_actions = null_ls.builtins.code_actions
      -- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
      -- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTIN_CONFIG.md
      null_ls.setup {
        sources = {
          --- general
          code_actions["refactoring"],
          --- golang
          diagnostics["golangci_lint"],
          code_actions["gomodifytags"],
          code_actions["impl"],
          formatting["gofumpt"],
          formatting["goimports"],
          --- python
          -- pyright does not provide support for formatting, so we use black/isort/ruff
          -- diagnostics["ruff"], -- FIXME: none-ls
          formatting["black"],
          formatting["isort"],
          -- formatting["ruff"],  -- FIXME: none-ls
          --- lua
          formatting["stylua"],
          -- javascript(react), typescript(react), vue
          -- code_actions["eslint_d"], -- FIXME: none-ls
          -- formatting["eslint_d"], -- FIXME: none-ls
          -- diagnostics["eslint_d"], -- FIXME: none-ls
          -- multiple
          formatting["prettier"].with {
            filetypes = {
              "css",
              "scss",
              "less",
              "html",
              "yaml",
              "markdown",
              "graphql",
            },
          },
          --- check on cmake
          diagnostics["cmake_lint"],
          --- git commit
          diagnostics["gitlint"],
          --- shells
          -- bashls does not support formatting
          formatting["shfmt"],
          -- make shell scripts beautiful
          -- code_actions["shellcheck"], -- FIXME: none-ls
          -- diagnostics["shellcheck"], -- FIXME: none-ls
          -- formatting["beautysh"], -- FIXME: none-ls
          -- simple check on zsh
          diagnostics["zsh"],
        },
      }
    end,
  },
  {
    "j-hui/fidget.nvim",
    tag = "legacy",
    config = function()
      require("fidget").setup()
    end,
  },

  {
    "neovim/nvim-lspconfig",
    config = function()
      local conf = require("plug.lsp.conf")
      local lspconfig = require("lspconfig")

      conf.general_setup()

      local simple_ls = {
        "jsonls", -- json (can do format so no need for jq)
        "marksman", -- markdown
        -- "neocmake", -- cmake
        "taplo", -- toml
        "texlab", -- latex
        "vimls", -- vim
        "yamlls", -- yaml
      }
      for _, ls in ipairs(simple_ls) do
        lspconfig[ls].setup {
          on_attach = conf.on_attach,
          capabilities = conf.capabilities,
        }
      end

      -- rust
      vim.g.rustaceanvim = {
        tools = {
          -- plugins
        },
        server = {
          on_attach = conf.on_attach,
          capabilities = conf.capabilities,
          settings = {
            ["rust-analyzer"] = {
              diagnostics = {
                experimental = true,
              },
            },
          },
        },
        dap = {
          -- dap
        },
      }

      -- golang
      lspconfig["gopls"].setup {
        on_attach = conf.on_attach,
        capabilities = conf.capabilities,
      }

      -- bash/zsh
      lspconfig["bashls"].setup {
        cmd_env = { GLOB_PATTERN = "*@(.sh|.inc|.bash|.command)" },
        filetypes = { "sh", "zsh", "bash" },
        on_attach = conf.on_attach,
        capabilities = conf.capabilities,
      }

      -- lua
      lspconfig["lua_ls"].setup {
        root_dir = lspconfig.util.root_pattern("init.lua", ".luarc.json", "stylua.toml", ".git"),
        on_attach = conf.on_attach,
        capabilities = conf.capabilities,
        -- https://github.com/sumneko/lua-language-server/wiki/Settings
        settings = {
          Lua = {
            runtime = {
              version = "LuaJIT",
              unicodeName = false,
              pathStrict = true,
            },
            semantic = {
              enable = true,
              annotation = true,
              keyword = false,
            },
            completion = {
              enable = true,
              postfix = "@",
              showWord = "Fallback",
              workspaceWord = true,
              autoRequire = true,
              displayContext = 3,
              callSnippet = "Replace",
              keywordSnippet = "Replace",
            },
            workspace = {
              -- library = vim.api.nvim_get_runtime_file("", true),
              checkThirdParty = false,
              preloadFileSize = require("core.utils").SizeInKiloBytes,
              maxPreload = 90000,
              ignoreDir = { ".vscode" },
              ignoreSubmodules = true,
            },
            -- https://github.com/LuaLS/lua-language-server/wiki/Diagnostics
            diagnostics = {
              enable = true,
              disable = {},
              disableScheme = { "git" },
              libraryFiles = "Disable",
            },
            hint = {
              enable = true,
              await = true,
              paramName = "Literal",
            },
            telemetry = {
              enable = false,
            },
            -- use stylua instead
            format = {
              enable = false,
            },
            hover = {
              enable = true,
            },
            type = {
              castNumberToInteger = true,
              weakNilCheck = false,
              weakUnionCheck = false,
            },
            window = {
              progressBar = true,
              statusBar = false,
            },
          },
        },
      }

      lspconfig["clangd"].setup {
        ---@param client vim.lsp.Client
        ---@param bufnr number
        on_attach = function(client, bufnr)
          local u = require("core.utils")
          conf.on_attach(client, bufnr)
          local bufopts = u.buf_opts(bufnr)
          u.keymap("n", "<localleader>dh", "<Cmd>ClangdTypeHierarchy<CR>", bufopts, "[clangd] show type hierarchy")
          u.keymap("n", "<localleader>di", "<Cmd>ClangdSymbolInfo<CR>", bufopts, "[clangd] show symbol info")
          u.keymap("n", "<localleader>ds", "<Cmd>ClangdSwitchSourceHeader<CR>", bufopts, "[clangd] switch .h/.cpp")
          u.keymap("n", "<localleader>dt", "<Cmd>ClangdAST<CR>", bufopts, "[clangd] show ast")
        end,
        capabilities = conf.make_capabilitites(function(c)
          c.offsetEncoding = "utf-8"
        end),
        -- don't use .clangd or .clang-format/.clang-tidy as root_dir patterns
        -- don't use .git as root pattern due to submodules
        root_dir = lspconfig.util.root_pattern("compile_commands.json"),
        cmd = {
          "clangd-17",
          "--all-scopes-completion=true",
          "--background-index=true",
          "--background-index-priority=normal",
          "--clang-tidy=true",
          "--completion-style=bundled",
          "--fallback-style=Google",
          "--header-insertion=iwyu",
          -- https://github.com/hrsh7th/nvim-cmp/issues/999
          "--header-insertion-decorators=false",
          "--limit-references=256",
          "--limit-results=50",
          "--pch-storage=disk",
          -- below are hidden options
          "--hidden-features",
          "--ranking-model=heuristics",
        },
      }

      -- prefer `pyproject.toml` to configure per project
      lspconfig["basedpyright"].setup {
        root_dir = lspconfig.util.root_pattern("pyproject.toml"),
        basedright = {
          analysis = {
            diagnosticMode = "openFilesOnly",
          },
        },
        -- pyright-specific
        handlers = {
          -- https://github.com/hrsh7th/nvim-cmp/issues/685#issuecomment-1002924899
          -- https://github.com/neovim/nvim-lspconfig/issues/726#issuecomment-1075539112
          ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
            signs = {
              severity = vim.diagnostic.severity.WARN,
            },
            virtual_text = {
              severity = vim.diagnostic.severity.WARN,
            },
          }),
        },
        on_attach = conf.on_attach,
        capabilities = conf.capabilities,
        settings = {
          python = {
            analysis = {
              -- use ruff for most of the checkings
              typeCheckingMode = "off",
            },
          },
        },
      }
    end,
  },

  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      debug = false,
    },
  },

  -- {
  --   "hongxuchen/xdev.nvim",
  --   lazy = true,
  --   event = "BufReadPost xmake.lua",
  --   config = true,
  --   dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
  -- },

  {
    "p00f/clangd_extensions.nvim",
    config = function()
      require("clangd_extensions").setup {
        autoSetHints = true,
        -- These apply to the default ClangdSetInlayHints command
        inlay_hints = {
          inline = true,
          -- Only show inlay hints for the current line
          only_current_line = false,
          -- Event which triggers a refersh of the inlay hints.
          -- You can make this "CursorMoved" or "CursorMoved,CursorMovedI" but
          -- not that this may cause higher CPU usage.
          -- This option is only respected when only_current_line and
          -- autoSetHints both are true.
          only_current_line_autocmd = "CursorHold",
          -- whether to show parameter hints with the inlay hints or not
          show_parameter_hints = true,
          -- prefix for parameter hints
          parameter_hints_prefix = "◀ ",
          -- prefix for all the other hints (type, chaining)
          other_hints_prefix = "⇛ ",
          -- whether to align to the length of the longest line in the file
          max_len_align = false,
          -- padding from the left if max_len_align is true
          max_len_align_padding = 1,
          -- whether to align to the extreme right or not
          right_align = false,
          -- padding from the right if right_align is true
          right_align_padding = 7,
          -- The color of the hints
          highlight = "DiagnosticVirtualTextHint",
          -- highlight = "Comment",
          -- The highlight group priority for extmark
          priority = 100,
        },
        ast = {
          role_icons = {
            type = "",
            declaration = "",
            expression = "",
            specifier = "",
            statement = "",
            ["template argument"] = "",
          },

          kind_icons = {
            Compound = "",
            Recovery = "",
            TranslationUnit = "",
            PackExpansion = "",
            TemplateTypeParm = "",
            TemplateTemplateParm = "",
            TemplateParamObject = "",
          },

          highlights = {
            detail = "Comment",
          },
        },
        memory_usage = {
          border = "none",
        },
        symbol_info = {
          border = "none",
        },
      }
    end,
  },
  {
    "mrcjkb/rustaceanvim",
    version = "^5", -- Recommended
    lazy = false, -- This plugin is already lazy
    dependencies = {
      { "saecki/crates.nvim", tag = "stable" },
    },
  },
  {
    "pmizio/typescript-tools.nvim",
    config = function()
      local conf = require("plug.lsp.conf")
      require("typescript-tools").setup {
        on_attach = function(client, bufnr)
          conf.on_attach(client, bufnr)
          -- use null_ls.prettier for formatting
          client.server_capabilities.documentFormattingProvider = false
          client.server_capabilities.documentRangeFormattingProvider = false
        end,
        settings = {
          expose_as_code_action = { "fix_all", "add_missing_imports", "remove_unused" },
          tsserver_file_preferences = {
            -- disableSuggestions = true,
          },
          tsserver_format_options = {},
        },
      }
    end,
  },
  {
    "Julian/lean.nvim",
    ft = "lean",
    config = function()
      local u = require("core.utils")
      local conf = require("plug.lsp.conf")
      ---@param client vim.lsp.Client
      ---@param bufnr number
      local my_lean_on_attach = function(client, bufnr)
        conf.on_attach(client, bufnr)
        local bufopts = u.buf_opts(bufnr)
        u.keymap(
          "n",
          "<localleader>dt",
          "<Cmd>LeanInfoviewToggle<CR>",
          bufopts,
          "[lean] toggle the infoview open or closed"
        )
        u.keymap(
          "n",
          "<localleader>dp",
          "<Cmd>LeanInfoviewPinTogglePause<CR>",
          bufopts,
          "[lean] pause the current infoview"
        )
        u.keymap("n", "<localleader>da", "<Cmd>LeanInfoviewAddPin<CR>", bufopts, "[lean] place an infoview pin")
        u.keymap(
          "n",
          "<localleader>dc",
          "<Cmd>LeanInfoviewClearPins<CR>",
          bufopts,
          "[lean] clear all current infoview pins"
        )
        u.keymap(
          "n",
          "<localleader>dsp",
          "<Cmd>LeanInfoviewSetDiffPin<CR>",
          bufopts,
          "[lean] place an infoview diff pin"
        )
        u.keymap(
          "n",
          "<localleader>dcp",
          "<Cmd>LeanInfoviewClearDiffPin<CR>",
          bufopts,
          "[lean] clear current infoview diff pin"
        )
        u.keymap(
          "n",
          "<localleader>dtp",
          "<Cmd>LeanInfoviewToggleAutoDiffPin<CR>",
          bufopts,
          "[lean] toggle auto diff pin mode"
        )
        u.keymap(
          "n",
          "<localleader>dtc",
          "<Cmd>LeanInfoviewToggleNoClearAutoDiffPin<CR>",
          bufopts,
          "[lean] toggle auto diff pin mode without clearing diff pin"
        )
        u.keymap("n", "<localleader>dw", "<Cmd>LeanInfoviewEnableWidgets<CR>", bufopts, "[lean] enable widgets")
        u.keymap("n", "<localleader>dW", "<Cmd>LeanInfoviewDisableWidgets<CR>", bufopts, "[lean] disable widgets")
        u.keymap("n", "<localleader><Tab>", "<Cmd>LeanGotoInfoview<CR>", bufopts, "[lean] goto infoview")
        u.keymap(
          "n",
          "<localleader>dsf",
          "<Cmd>LeanSorryFill<CR>",
          bufopts,
          "[lean] replace a 'try this:' suggestion under the cursor"
        )
        u.keymap(
          "n",
          "<localleader>dtt",
          "<Cmd>LeanTryThis<CR>",
          bufopts,
          "[lean] jump into the infoview window associated with the current lean file"
        )
        u.keymap(
          "n",
          "<localleader>d\\",
          "<Cmd>LeanAbbreviationsReverseLookup<CR>",
          bufopts,
          "[lean] show what abbreviation produces the symbol under the cursor"
        )
      end

      -- lean4
      require("lean").setup {
        abbreviations = { builtin = true },
        lsp = { on_attach = my_lean_on_attach },
        lsp3 = { on_attach = my_lean_on_attach },
        mappings = false,
      }
    end,
  },
}
