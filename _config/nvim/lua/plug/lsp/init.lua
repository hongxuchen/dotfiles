return {
  {
    "linrongbin16/lsp-progress.nvim",
    enabled = false,
    config = function()
      require("lsp-progress").setup {}
    end,
  },

  {
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup {}
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
        "neocmake", -- cmake
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
          code_action = {
            ui_select_fallback = true,
          },
          -- plugins
        },
        server = {
          root_dir = lspconfig.util.root_pattern(
            "README.md",
            ".git",
            "rustfmt.toml",
            "Makefile.toml",
            "clippy.toml",
            "rust-toolchain.toml",
            "rust-toolchain",
            "build.rs",
            ".editorconfig",
            "LICENSE"
          ),
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
      -- TODO: duplication of conform+nvim-lint
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
          u.keymap("n", "<localleader>dm", "<Cmd>ClangdMemoryUsage<CR>", bufopts, "[clangd] show memory usage")
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
          "--limit-results=80",
          "--pch-storage=disk",
          -- below are hidden options
          "--hidden-features",
          "--ranking-model=heuristics",
        },
      }

      --- python settings
      -- prefer `pyproject.toml` to configure per project
      lspconfig["basedpyright"].setup {
        root_dir = lspconfig.util.root_pattern("pyproject.toml"),
        basedright = {
          analysis = {
            diagnosticMode = "openFilesOnly",
            autoSearchPaths = true,
            useLibraryCodeForTypes = true,
          },
        },
        -- basedpyright-specific
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
      lspconfig.ruff.setup {}
    end,
  },

  -- lua
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      debug = false,
      library = {
        { path = "luvit-meta/library", words = { "vim%.uv" } },
        { path = "wezterm-types", mods = { "wezterm" } },
      },
    },
    dependencies = {
      { "Bilal2453/luvit-meta", lazy = true },
      { "justinsgithub/wezterm-types", lazy = true },
    },
  },

  -- c/c++
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

  -- rust
  {
    "mrcjkb/rustaceanvim",
    version = "^5", -- Recommended
    lazy = false, -- This plugin is already lazy
    dependencies = {
      {
        "saecki/crates.nvim",
        config = function()
          require("crates").setup {
            lsp = {
              enabled = true,
              actions = true,
              completion = true,
              hover = true,
            },
          }
        end,
      },
    },
  },

  -- typescript
  {
    "pmizio/typescript-tools.nvim",
    enabled = false,
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
}
