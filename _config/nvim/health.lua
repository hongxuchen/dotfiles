local M = {}

-- TODO: used as a C/C++ IDE plugin

M.bin_clangd = "clangd-17"
M.bin_ast_grep = "ast-grep"
M.bin_delta = "delta"
M.bin_zoxide = "zoxide"
M.bin_chafa = "chafa"

---@type string[]
local executables = { M.bin_clangd, M.bin_delta, M.bin_ast_grep, M.bin_zoxide, M.bin_chafa }

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
