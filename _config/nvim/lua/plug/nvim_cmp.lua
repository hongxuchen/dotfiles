return {
  -- "hrsh7th/nvim-cmp",
  -- https://github.com/hrsh7th/nvim-cmp/pull/1980
  "hrsh7th/nvim-cmp",
  enabled = false,
  event = { "InsertEnter" },
  dependencies = {
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-buffer",
    -- don't use `spell` as source since `which-key.nvim` already provides it
    "uga-rosa/cmp-dictionary",
    "hrsh7th/cmp-nvim-lsp",
    -- don't use cmp-nvim-lsp-signature-help since for most of time we have snippets
    { "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
    "saadparwaiz1/cmp_luasnip",
  },
  config = function()
    local luasnip = require("luasnip")
    -- https://www.reddit.com/r/neovim/comments/12z0orb/unexpected_behavior_when_pressing_tab_in_insert/
    -- see also https://github.com/L3MON4D3/LuaSnip/issues/656
    luasnip.config.set_config {
      enable_autosnippets = false,
      region_check_events = "InsertEnter",
      delete_check_events = "InsertLeave",
      history = false,
    }

    local u = require("core.utils")
    local cmp = require("cmp") -- nvim-cmp

    -- https://github.com/hrsh7th/nvim-cmp/blob/main/lua/cmp/config/compare.lua
    local compare = require("cmp.config.compare")

    local types = require("cmp.types")
    ---@type table<integer, integer>
    local modified_priority = {
      [types.lsp.CompletionItemKind.Variable] = types.lsp.CompletionItemKind.Method,
      [types.lsp.CompletionItemKind.Snippet] = 0, -- top
      [types.lsp.CompletionItemKind.Keyword] = 0, -- top
      [types.lsp.CompletionItemKind.Text] = 100, -- bottom
    }
    ---@param kind integer: kind of completion entry
    local function modified_kind(kind)
      return modified_priority[kind] or kind
    end

    -- score by lsp, if available; this includes builtin sort_text
    ---@param entry1 cmp.Entry
    ---@param entry2 cmp.Entry
    ---@return boolean|nil
    function compare.my_lsp(entry1, entry2)
      -- check deprecation
      local e1_deprecated = entry1:is_deprecated()
      local e2_deprecated = entry2:is_deprecated()
      if not e1_deprecated and e2_deprecated then
        return true
      elseif e1_deprecated and not e2_deprecated then
        return false
      end

      -- check underscore
      local _, e1_under = entry1.completion_item.label:find("^_+")
      local _, e2_under = entry2.completion_item.label:find("^_+")
      e1_under = e1_under or 0
      e2_under = e2_under or 0
      if e1_under > e2_under then
        return false
      elseif e1_under < e2_under then
        return true
      end

      -- check lsp kind
      local kind1 = modified_kind(entry1:get_kind())
      local kind2 = modified_kind(entry2:get_kind())
      if kind1 ~= kind2 then
        return kind1 - kind2 < 0
      end

      -- sort text
      local t1 = entry1.completion_item.sortText
      local t2 = entry2.completion_item.sortText
      if t1 ~= nil and t2 ~= nil and t1 ~= t2 then
        return t1 < t2
      end
    end

    local config_luasnip = {
      name = "luasnip",
      keyword_length = 1,
      priority = 100,
      group_index = 1,
      option = {
        show_autosnippets = false,
        use_show_condition = true,
      },
    }

    local config_lazydev = {
      name = "lazydev",
      group_index = 0,
      keyword_length = 2,
    }

    local config_nvim_lsp = {
      name = "nvim_lsp",
      priority = 100,
      group_index = 1,
      keyword_length = 1,
      -- trigger_characters = { "." },
      ---@param entry cmp.Entry
      ---@param ctx cmp.Context
      ---@return boolean|nil
      entry_filter = function(entry, ctx)
        -- clangd treats macros as 'Text'
        if ctx.filetype == "cpp" or ctx.filetype == "c" then
          return true
        end
        return types.lsp.CompletionItemKind[entry:get_kind()] ~= "Text"
      end,
    }

    local config_dictionary = {
      name = "dictionary",
      priority = 10,
      group_index = 2,
      -- trigger_characters = { "@" },
      keyword_length = 3,
      max_item_count = 6,
    }

    local config_path = {
      name = "path",
      priority = 10,
      group_index = 2,
      keyword_length = 3,
      option = {
        trailing_slash = false,
        label_trailing_slash = true,
      },
    }

    local config_buffer = {
      name = "buffer",
      priority = 1,
      group_index = 3,
      option = {
        keyword_length = 3,
        keyword_pattern = [[\k\+]],
        get_bufnrs = function()
          ---@type integer
          local bufnr = vim.api.nvim_get_current_buf()
          local is_bigfile = require("core.utils").is_bigfile(bufnr)
          if is_bigfile then
            return {}
          end
          return { bufnr }
        end,
      },
    }

    cmp.setup {
      preselect = cmp.PreselectMode.None,
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      formatting = {
        expandable_indicator = true,
        fields = { "abbr", "menu", "kind" },
        format = function(entry, item)
          local menu_icon = {
            nvim_lsp = "λ",
            luasnip = "⋗",
            buffer = "Ω",
            path = "",
            dictionary = "",
          }
          item.menu = menu_icon[entry.source.name]
          return item
        end,
      },
      sorting = {
        priority_weight = 2,
        -- https://www.reddit.com/r/neovim/comments/14k7pbc/what_is_the_nvimcmp_comparatorsorting_you_are
        comparators = {
          compare.exact, -- whether is exact match
          compare.my_lsp, -- lsp related
          compare.score, -- score is calculated with priority
          compare.recently_used, -- recently_used
          compare.length, -- label length smaller is better
          compare.offset, -- offset
          compare.scopes, -- scopes
          compare.locality, -- locality
          compare.order, -- compares cmp.Entry.id, lowest priority
        },
      },
      performance = {
        async_budget = 4,
        confirm_resolve_timeout = 100,
        debounce = 60,
        fetching_timeout = 2000,
        max_view_entries = 50,
        throttle = 30,
      },
      mapping = cmp.mapping.preset.insert {
        ["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
        ["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
        ["<C-u>"] = cmp.mapping.scroll_docs(-4),
        ["<C-d>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete {},
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"] = cmp.mapping.confirm {
          behavior = cmp.ConfirmBehavior.Insert,
          select = true,
        },
        ["<S-CR>"] = cmp.mapping.confirm {
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        },
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          elseif u.has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
      },
      -- https://github.com/hrsh7th/nvim-cmp/wiki/List-of-sources
      sources = {
        config_lazydev,
        config_luasnip,
        config_nvim_lsp,
        config_path,
        config_dictionary,
        config_buffer,
      },
      completion = {
        keyword_length = 2,
        -- completeopt = "menu,menuone,noinsert",
      },
      matching = {
        disallow_fuzzy_matching = true,
        disallow_fullfuzzy_matching = true,
        disallow_partial_fuzzy_matching = true,
        disallow_partial_matching = false,
        disallow_prefix_unmatching = true,
        disallow_symbol_nonprefix_matching = true,
      },
      window = {
        -- completion = cmp.config.window.bordered { border = "none" },
        -- disable documentation window
        documentation = cmp.config.window.bordered(),
        -- documentation = cmp.config.disable,
      },
      experimental = {
        ghost_text = false,
      },
    }

    cmp.setup.filetype({ "text", "markdown" }, {
      sources = {
        config_dictionary,
        config_path,
        config_buffer,
      },
    })

    cmp.setup.filetype({
      "c",
      "cpp",
      "python",
      "go",
    }, {
      sources = {
        config_luasnip,
        config_nvim_lsp,
        config_dictionary,
        config_buffer,
      },
    })

    -- autopair compability
    local cmp_autopairs = require("nvim-autopairs.completion.cmp")
    cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done {})

    -- crates.nvim
    u.au({ "BufNewFile", "BufReadPost" }, {
      group = u.ag("CmpSourceCargo", { clear = true }),
      pattern = "Cargo.toml",
      callback = function()
        local config_crates = {
          name = "crates",
          group_index = 1,
          priority = 100,
          keyword_length = 2,
        }
        cmp.setup.buffer {
          sources = { config_crates, config_path, config_dictionary, config_buffer },
        }
      end,
    })

    -- cmp-dictionary source
    local dict = require("cmp_dictionary")
    dict.setup {
      exact_length = 2,
      async = true,
      sqlite = false,
    }
    -- dict.switcher {
    --   spelllang = {
    --     en = require("core.utils").en_dict,
    --   },
    -- }

    -- otherwise it is too slow on large files
    vim.api.nvim_clear_autocmds {
      event = { "CmdlineEnter", "CmdlineChanged", "CmdlineLeave" },
      pattern = "*",
      group = "___cmp___",
    }
  end,
}
