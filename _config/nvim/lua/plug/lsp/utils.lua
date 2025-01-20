M = {}

local u = require("core.utils")

function M.detach_all_clients_buf(buf)
  local clients = vim.lsp.get_clients { bufnr = buf }
  for _, client in pairs(clients) do
    vim.lsp.buf_detach_client(buf, client.id)
  end
end

function M.disable_semantic_highlight()
  ---@type string[]
  local highlights = vim.fn.getcompletion("@lsp", "highlight")
  for _, group in ipairs(highlights) do
    vim.api.nvim_set_hl(0, group, {})
  end
end

function M.is_client_active(name)
  local clients = vim.lsp.get_clients()
  return u.find_first(clients, function(client)
    return client.name == name
  end)
end

---@param server_name string can be any server supported by lspconfig
---@return string[] supported filestypes as a list of strings
function M.get_supported_filetypes(server_name)
  local status_ok, config = pcall(require, ("lspconfig.server_configurations.%s"):format(server_name))
  if not status_ok then
    vim.notify(string.format(string.format("%s config not found", server_name)), vim.log.levels.WARN)
    return {}
  end
  return config.default_config.filetypes or {}
end

---list capabilities of the current buffer's associated LSP client
---@param verbose boolean whether dump verbose capabilities
function M.list_capabilities(verbose)
  local bufnr = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients { bufnr = bufnr }
  -- TODO: ensure there is only one non null-ls LS and highlight '-' capabilities
  for _, client in pairs(clients) do
    if client.name ~= "null-ls" then
      if verbose then
        u.display_lua_obj(client.server_capabilities)
      else
        local caps = {}
        ---@param capability string
        ---@param info boolean
        for capability, info in pairs(client.server_capabilities) do
          if capability:find("Provider") then
            local capability_str = capability:gsub("Provider$", "")
            if info then
              table.insert(caps, "+ " .. capability_str)
            else
              table.insert(caps, "- " .. capability_str)
            end
          end
        end
        table.sort(caps)
        local msg = "# " .. client.name .. "\n" .. table.concat(caps, "\n")
        vim.print(msg)
      end
    end
  end
end

--- list attached lsp buffers, starting from current buffer
function M.list_attached_buffers()
  local bufnr = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients { bufnr = bufnr }
  if #clients == 0 then
    vim.notify(
      string.format("no lsp clients for %s (%d)", vim.api.nvim_buf_get_name(bufnr), bufnr),
      vim.log.levels.WARN,
      {}
    )
    return
  end
  local notified = false
  local null_ls = nil
  for _, client in pairs(clients) do
    if client.name ~= "null-ls" then
      local bufnrs = vim.lsp.get_buffers_by_client_id(client.id)
      local number = #bufnrs
      local buffers = vim.tbl_map(function(nr)
        return string.format("%6d  %s", nr, vim.api.nvim_buf_get_name(nr))
      end, bufnrs)
      if number == 0 then
        vim.notify(string.format("no attached buffers to: %s (%d)", client.name, client.id), vim.log.levels.WARN, {})
        notified = true
      else
        table.sort(buffers)
        local msg = string.format(
          "# %s (ClientID=%d), %d buffer(s) attached:\n%s",
          client.name,
          client.id,
          number,
          table.concat(buffers, "\n")
        )
        vim.notify(msg, vim.log.levels.TRACE)
        notified = true
      end
    else
      null_ls = client
    end
    if not notified then
      if null_ls == nil then
        vim.notify("no language servers attached", vim.log.levels.WARN, {})
      else
        vim.notify(string.format("Only null-ls is attached (clientID=%d)", null_ls.id), vim.log.levels.INFO, {})
      end
    end
  end
end

function M.qf_handler(result)
  local entries = {}
  local num_files, num_updates = 0, 0
  for uri, edits in pairs(result.changes) do
    num_files = num_files + 1
    local bufnr = vim.uri_to_bufnr(uri)

    for _, edit in ipairs(edits) do
      local start_line = edit.range.start.line + 1
      local line = vim.api.nvim_buf_get_lines(bufnr, start_line - 1, start_line, false)[1]

      num_updates = num_updates + 1
      table.insert(entries, {
        bufnr = bufnr,
        lnum = start_line,
        col = edit.range.start.character + 1,
        text = line,
      })
    end
  end

  if num_files > 1 then
    u.qf_populate(entries, "r")
  end
end

function M.direct_save_handler(result)
  local cur_bufnr = vim.api.nvim_get_current_buf()
  for uri, _ in pairs(result.changes) do
    local bufnr = vim.uri_to_bufnr(uri)
    if bufnr ~= cur_bufnr then
      vim.api.nvim_buf_call(bufnr, function()
        vim.cmd("silent update!")
      end)
    end
  end
end

-- TODO: make it within "textDocument/rename" handler
-- FIXME: not work for python
function M.my_lsp_rename()
  local params = vim.lsp.util.make_position_params(0, "utf-16")
  params.oldName = u.word_under_cursor()

  vim.ui.input({ prompt = "New Name: ", default = params.oldName }, function(input)
    if input == nil then
      vim.notify(string.format("[lsp] abort renaming: %s", params.oldName), vim.log.levels.WARN)
      return
    elseif input == params.oldName then
      vim.notify(string.format("[lsp] name unchanged: %s", params.oldName), vim.log.levels.WARN)
    end
    params.newName = input
    vim.lsp.buf_request(0, "textDocument/rename", params, function(err, result, ...)
      if not result then
        vim.notify(
          string.format("[lsp] no results to rename %s->%s", params.oldName, params.newName),
          vim.log.levels.WARN
        )
        return
      end
      if not result or not result.changes then
        vim.notify(
          string.format("[lsp] no changes to rename %s->%s", params.oldName, params.newName),
          vim.log.levels.WARN
        )
        return
      end
      vim.lsp.handlers["textDocument/rename"](err, result, ...)
      M.direct_save_handler(result)
    end)
  end)
end

return M
