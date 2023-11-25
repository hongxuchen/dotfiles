local M = {}

local u = require("core.utils")

---@param override fun(lsp.capabilities)|nil -> nil
---@return lsp.ClientCapabilities
function M.make_capabilitites(override)
  local cap = vim.lsp.protocol.make_client_capabilities()
  cap.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
  }
  if override then
    override(cap)
  end
  local cmp_cap = require("cmp_nvim_lsp").default_capabilities()
  return vim.tbl_deep_extend("force", cap, cmp_cap)
end

M.capabilities = M.make_capabilitites()

function M.general_setup()
  -- general diagnostics
  for type, icon in pairs(u.signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
  end

  vim.diagnostic.config {
    virtual_text = {
      source = "if_many",
      prefix = "●",
    },
    float = {
      source = "if_many",
      focusable = false,
      border = "none",
      style = "minimal",
    },
    signs = true,
    underline = true,
    update_in_insert = false,
    serverity_sort = true,
  }

  -- hover
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "none",
    focusable = true,
    max_width = 120,
    max_height = 25,
  })

  ---lua_ls treats `f = function() ... end`(lnum+filename usually the same) as two defs, pick one
  ---@param _ any
  ---@param result nil|table (`Location`|`LocationLink`)
  ---@param ctx lsp.HandlerContext
  vim.lsp.handlers["textDocument/definition"] = function(_, result, ctx)
    ---@type lsp.Client?
    local client = vim.lsp.get_client_by_id(ctx.client_id)
    assert(client ~= nil)
    if not result or vim.tbl_isempty(result) then
      vim.notify(string.format("[LSP] no defintion found by %s", client.name), vim.log.levels.WARN)
      return
    end
    if vim.tbl_islist(result) then
      local results = vim.lsp.util.locations_to_items(result, client.offset_encoding)
      local lnum, filename = results[1].lnum, results[1].filename
      for _, val in pairs(results) do
        if val.lnum ~= lnum or val.filename ~= filename then
          require("telescope.builtin").lsp_definitions()
          return
        end
      end
      vim.lsp.util.jump_to_location(result[1], client.offset_encoding, false)
    else
      vim.lsp.util.jump_to_location(result, client.offset_encoding, false)
    end
  end

  -- signatureHelp
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "none" })

  local du = require("plug.lsp.utils")

  -- user defined commands
  local mycmd = vim.api.nvim_create_user_command
  mycmd("LspBuffers", du.list_attached_buffers, {})
  mycmd("LspCapabilities", function(info)
    du.list_capabilities(info.bang)
  end, { bang = true })
  mycmd("LspClientsByFT", function(info)
    local filetype = info.args
    local res = require("lspconfig.util").get_active_clients_list_by_ft(filetype)
    vim.print(res)
  end, { nargs = 1, complete = "filetype" })
  mycmd("LspConfigByFT", function(info)
    local filetype = info.args
    local res = require("lspconfig.util").get_config_by_ft(filetype)
    u.display_lua_obj(res)
  end, { nargs = 1, complete = "filetype" })
end

-- When the current buffer does not attach a LS, there is no binding for the buffer, so we have to bind globally
u.keymap("n", "<localleader>li", "<Cmd>LspInfo<CR>", u.opts, "[lspconfig] show lsp info")
u.keymap("n", "<localleader>ll", "<Cmd>LspLog<CR>", u.opts, "[lspconfig] show lsp logs")

-- NOTE: '<localleader>d' is reserved for language-specific mappings
---@param client lsp.Client
---@param bufnr number
function M.on_attach(client, bufnr)
  -- neovim nightly seems to have bug on inlay_hints, so comment out
  -- local no_inlayhints_servers = { "clangd", "lua_ls" }
  -- if client.server_capabilities.inlayHintProvider then
  --   if vim.list_contains(no_inlayhints_servers, client.name) then
  --     vim.lsp.inlay_hint(bufnr, false)
  --   else
  --     vim.lsp.inlay_hint(bufnr, true)
  --   end
  -- end

  local t_builtin = require("telescope.builtin")

  local bufopts = u.buf_opts(bufnr)
  -- format with =
  -- vim.api.nvim_buf_set_option(bufnr, "formatexpr", "v:lua.vim.lsp.formatexpr(#{timeout_ms:250})")
  u.keymap("n", "gD", vim.lsp.buf.declaration, bufopts, "[lsp] go to declaration")
  -- FIXME: sometimes jumps to random position at first
  -- NOTE: handler is modified
  u.keymap("n", "gd", vim.lsp.buf.definition, bufopts, "[lsp] go to definition")
  u.keymap("n", "gi", t_builtin.lsp_implementations, bufopts, "[lsp] go to implementation")
  u.keymap("n", "gr", t_builtin.lsp_references, bufopts, "[lsp] go to references")
  u.keymap("n", "<localleader>gt", t_builtin.lsp_type_definitions, bufopts, "[lsp] go to type definition")
  u.keymap("n", "K", vim.lsp.buf.hover, bufopts, "[lsp] get hover")
  u.keymap("i", "<C-s>", vim.lsp.buf.signature_help, bufopts, "[lsp] get signature help")

  u.keymap("n", "[d", vim.diagnostic.goto_prev, bufopts, "[vim] go to previous diagnostic")
  u.keymap("n", "]d", vim.diagnostic.goto_next, bufopts, "[vim] go to next diagnostic")

  u.keymap("n", "<localleader>ci", t_builtin.lsp_incoming_calls, bufopts, "[lsp] go to incoming_calls")
  u.keymap("n", "<localleader>co", t_builtin.lsp_outgoing_calls, bufopts, "[lsp] go to outgoing_calls")
  u.keymap("n", "<localleader>ca", vim.lsp.buf.code_action, bufopts, "[lsp] code action")
  u.keymap("n", "<localleader>cf", function()
    vim.lsp.buf.format { async = true }
  end, bufopts, "[lsp] format buffer")
  u.keymap("n", "<localleader>cu", vim.lsp.codelens.refresh, bufopts, "[lsp] refresh codelens")
  u.keymap("n", "<localleader>cr", vim.lsp.codelens.run, bufopts, "[lsp] run codelens")

  u.keymap("n", "<localleader>if", vim.diagnostic.open_float, bufopts, "[vim] open a float window about a diagnostic")
  u.keymap("n", "<localleader>ic", vim.diagnostic.setloclist, bufopts, "[vim] show diagnostics in loclist")
  u.keymap("n", "<localleader>id", function()
    t_builtin.diagnostics { bufnr = 0 }
  end, bufopts, "[vim] show diagnostics in telescope")

  u.keymap("n", "<localleader>ws", function()
    t_builtin.lsp_workspace_symbols { query = u.word_under_cursor() }
  end, bufopts, "[lsp] search current symbol in workspace")
  u.keymap(
    "n",
    "<localleader>wd",
    t_builtin.lsp_dynamic_workspace_symbols,
    bufopts,
    "[lsp] search dynamic workspace symbol"
  )
  u.keymap("n", "<localleader>wl", function()
    vim.print(vim.lsp.buf.list_workspace_folders())
  end, bufopts, "[lsp] list workspace folders")
  u.keymap("n", "<localleader>we", function()
    vim.diagnostic.setqflist { severity = "E" }
  end, bufopts, "[vim] show all errors in workspace")
  u.keymap("n", "<localleader>ww", function()
    vim.diagnostic.setqflist { severity = "W" }
  end, bufopts, "[vim] show all warnings in workspace")
  u.keymap("n", "<localleader>wa", vim.lsp.buf.add_workspace_folder, bufopts, "[lsp] add to workspace folder")
  u.keymap("n", "<localleader>wr", vim.lsp.buf.remove_workspace_folder, bufopts, "[lsp] remove from workspace folder")

  u.keymap("n", "<localleader>rn", function()
    require("plug.lsp.utils").my_lsp_rename()
  end, bufopts, "[lsp] rename refactor")
end

return M
