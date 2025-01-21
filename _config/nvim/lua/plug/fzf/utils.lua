local M = {}

---@type fun(name: string?): table<string, any>
M.get_lazy_plugins = (function()
  local plugins
  return function(name)
    if not plugins then
      -- https://github.com/folke/lazy.nvim/blob/d3974346b6cef2116c8e7b08423256a834cb7cbc/lua/lazy/view/render.lua#L38-L40
      local cfg = package.loaded["lazy.core.config"]
      plugins = vim.tbl_extend("keep", {}, cfg.plugins, cfg.to_clean, cfg.spec.disabled)
      -- kind="clean" seems not named in table
      for i, p in ipairs(plugins) do
        plugins[p.name] = p
        plugins[i] = nil
      end
    end
    if name then
      return plugins[name]
    end
    return plugins
  end
end)()

function M.zoxide_chdir(path)
  vim.system { "zoxide", "add", path }
  return vim.api.nvim_set_current_dir(path)
end

function M.zoxide_list()
  local res = vim.system({ "zoxide", "query", "-l" }, { text = true }):wait()
  if res.code == 0 then
    local l = vim.split(res.stdout, "\n", { plain = true, trimempty = true })
    return l
  else
    return {}
  end
end

return M
