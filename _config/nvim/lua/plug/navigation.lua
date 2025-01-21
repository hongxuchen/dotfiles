return {
  "stevearc/aerial.nvim",
  keys = {
    {
      "<localleader>ss",
      "<Cmd>AerialToggle<CR>",
      desc = "[aerial] toggle symbol outline",
    },
    {
      "<localleader>sb",
      function()
        local bufnr = vim.api.nvim_get_current_buf()
        local au = require("aerial.util")
        if au.is_aerial_buffer(bufnr) then
          local source_bufnr = au.get_source_buffer(bufnr)
          if source_bufnr then
            bufnr = source_bufnr
            vim.cmd(string.format("wincmd p | buffer %d", bufnr))
          else
            vim.notify("[aerial] source buffer not found", vim.log.levels.WARN)
            return
          end
        end

        local aerial = require("aerial")
        local data = aerial.info()
        local attached_backend = ""
        local backends = {}
        for _, status in ipairs(data.backends) do
          if status.attached then
            attached_backend = status.name
          elseif status.supported then
            table.insert(backends, status.name)
          end
        end
        if attached_backend == "" then
          vim.notify("no available backends", vim.log.levels.WARN)
          return
        end
        if #backends == 0 then
          vim.notify(attached_backend .. " is the only backend available", vim.log.levels.WARN)
          return
        end
        -- put currently attached at rightmost
        table.insert(backends, attached_backend)
        vim.b[bufnr].aerial_backends = backends
        aerial.refetch_symbols(bufnr)
        vim.notify(string.format("[aerial] backends: %s", vim.inspect(backends)))
      end,
      desc = "[aerial] switch backends",
    },
  },
  config = function()
    local default_symbol_kind = {
      "Class",
      "Constructor",
      "Enum",
      "Function",
      "Interface",
      "Method",
      "Module",
      "Struct",
    }

    local lua_symbol_kind = {
      "Class",
      "Constructor",
      "Enum",
      "Event",
      "Function",
      "Interface",
      "Method",
      "Module",
      "Namespace",
      -- "Package", -- lua_ls wrongly treat as if/elseif/else
      "Struct",
    }

    local python_symbol_kind = {
      "Class",
      "Constructor",
      "Enum",
      "Event",
      "Function",
      "Interface",
      "Method",
      "Module",
      "Namespace",
      "Package",
      "Struct",
    }

    local aerial = require("aerial")
    require("telescope").load_extension("aerial")

    local u = require("core.utils")
    aerial.setup {
      -- optionally use on_attach to set keymaps when aerial has attached to a buffer
      on_attach = function(bufnr)
        local bufopts = u.buf_opts(bufnr)
        u.keymap("n", "{", "<Cmd>AerialPrev<CR>", bufopts, "[aerial] prev symbol")
        u.keymap("n", "}", "<Cmd>AerialNext<CR>", bufopts, "[aerial] next symbol")
      end,
      backends = {
        ["_"] = { "lsp", "treesitter" },
        ["cpp"] = { "lsp"},
        markdown = { "markdown", "treesitter" },
        man = { "man" },
      },
      manage_folds = true,
      link_folds_to_tree = true,
      filter_kind = {
        ["_"] = default_symbol_kind,
        ["c"] = false,
        ["cpp"] = false,
        ["lua"] = lua_symbol_kind,
        ["python"] = python_symbol_kind,
      },
      show_guides = true,
      layout = {
        max_width = { 40, 0.25 },
        min_width = { 32, 0.25 },
        width = nil,
        default_direction = "prefer_right",
        placement = "edge",
        resize_to_content = true,
        preserve_equality = false,
      },
      lazy_load = true,
      disable_max_lines = u.Lines,
      disable_max_size = u.SizeInBytes,
      highlight_on_hover = true,
      highlight_closest = true,
      highlight_on_jump = false,
      nerd_font = "auto",
      lsp = {
        diagnostics_trigger_update = false,
        update_delay = 300,
        update_when_errors = true,
      },
      treesitter = {
        update_delay = 300,
      },
    }
  end,
}
