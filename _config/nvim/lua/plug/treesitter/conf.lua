local M = {}

function M.get_modules()
  local ts_config = require("nvim-treesitter.configs")
  return ts_config.available_modules()
end

return M
