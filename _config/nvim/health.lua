local M = {}

-- TODO: used as a C/C++ IDE plugin

---@type string[]
local executables = { "clangd" }

---@type string[]
local envs = { "NVIM_CONFIG", "EMAIL", "AUTHOR" }

function M.check()
  local not_installed = {}
  for _, exec in ipairs(executables) do
    if vim.fn.executable(exec) ~= 1 then
      table.insert(not_installed, exec)
    end
  end
  if #not_installed == 0 then
    vim.health.ok(string.format("all installed: ", vim.inspect(executables)))
  else
    vim.health.error(string.format("not installed: %s", vim.inspect(not_installed)))
  end

  for _, env in ipairs(envs) do
    ---@type string|nil
    local value = vim.env[env]
    if not value then
      vim.health.error(string.format("$%s not set", env))
    else
      vim.health.ok(string.format("$%s: %s", env, value))
    end
  end
end

return M
