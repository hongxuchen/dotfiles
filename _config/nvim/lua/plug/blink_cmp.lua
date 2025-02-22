---@type LazyPluginSpec
return {
  "saghen/blink.cmp",
  enabled = true,
  lazy = false, -- lazy loading handled internally
  -- optional: provides snippets for the snippet source
  dependencies = {
    -- { "Saghen/blink.compat" },
    { "rafamadriz/friendly-snippets" },
    { "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
  },

  -- use a release tag to download pre-built binaries
  version = "v0.*",
  -- OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
  -- build = 'cargo build --release',

  ---@type blink.cmp.Config
  opts = {
    snippets = {
      expand = function(snippet)
        require("luasnip").lsp_expand(snippet)
      end,
      active = function(filter)
        if filter and filter.direction then
          return require("luasnip").jumpable(filter.direction)
        end
        return require("luasnip").in_snippet()
      end,
      jump = function(direction)
        require("luasnip").jump(direction)
      end,
    },
    appearance = {
      -- sets the fallback highlight groups to nvim-cmp's highlight groups
      -- useful for when your theme doesn't support blink.cmp
      -- will be removed in a future release, assuming themes add support
      use_nvim_cmp_as_default = false,
      -- set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
      -- adjusts spacing to ensure icons are aligned
      nerd_font_variant = "mono",
    },
    completion = {
      keyword = { range = "prefix" },
      accept = {
        auto_brackets = {
          enabled = false,
          kind_resolution = {
            enabled = true,
            blocked_filetypes = { "cpp", "c", "typescriptreact", "javascriptreact", "vue" },
          },
        },
      },
      list = {
        selection = {
          preselect = true,
          -- preselect = function(ctx)
          --   return ctx.mode ~= "cmdline" and not require("blink.cmp").snippet_active { direction = 1 }
          -- end,
          auto_insert = true,
        },
      },
      menu = {
        draw = {
          treesitter = { "lsp" },
        },
      },
      documentation = {
        auto_show = true,
        auto_show_delay_ms = 100,
        update_delay_ms = 50,
        window = {
          border = "rounded",
          winblend = vim.o.pumblend,
        },
      },
      ghost_text = {
        enabled = false,
      },
    },

    signature = {
      -- disable
      enabled = false,
      window = { border = "rounded" },
    },

    sources = {
      default = { "lsp", "snippets", "buffer" },
      per_filetype = {
        markdown = { "markdown", "buffer" },
        lua = { "lsp", "snippets", "buffer", "lazydev" },
      },
      providers = {
        lsp = {
          name = "LSP",
          fallbacks = {
            "lazydev",
          },
          score_offset = 10,
        },
        lazydev = {
          name = "LazyDev",
          module = "lazydev.integrations.blink",
          score_offset = 10,
        },
        markdown = {
          name = "RenderMarkdown",
          module = "render-markdown.integ.blink",
          fallbacks = { "lsp" },
          score_offset = 5,
        },
        path = {
          min_keyword_length = 0,
          score_offset = 1,
        },
        buffer = {
          min_keyword_length = 3,
          max_items = 5,
          score_offset = 1,
        },
        snippets = {
          min_keyword_length = 2,
          score_offset = 10,
        },
      },
      min_keyword_length = 0,
    },

    cmdline = {
      enabled = false,
    },

    keymap = {
      preset = "enter",
      ["<C-y>"] = { "select_and_accept" },
    },
  },
}
