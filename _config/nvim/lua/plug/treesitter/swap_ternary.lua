---@diagnostic disable: param-type-mismatch, undefined-field
local ts = require("vim.treesitter")
local ts_utils = require("nvim-treesitter.ts_utils")
local parsers = require("nvim-treesitter.parsers")

-- TODO: add incommand preview

local M = {}

---@param node TSNode?
---@return TSNode?
local function get_ternary_node(node)
  if not node then
    return nil
  end

  if node:type() ~= "conditional_expression" then
    node = node:parent()
    return get_ternary_node(node)
  end

  return node
end

function M.swap_ternary()
  local bufnr = vim.api.nvim_get_current_buf()
  local lang = parsers.get_buf_lang(bufnr)

  if not parsers.has_parser(lang) then
    vim.notify("No treesitter parser for language: " .. lang, vim.log.levels.WARN, {})
    return
  end

  local node = ts_utils.get_node_at_cursor(0)

  local ternary_node = get_ternary_node(node)

  if not ternary_node then
    vim.notify("No ternary expression found", vim.log.levels.WARN, {})
    return
  end

  local t_consequence = ternary_node:field("consequence")
  local result = ""
  local t_alternate = ternary_node:field("alternative")
  for i = 1, #t_alternate do
    local text = ts.get_node_text(t_alternate[i], bufnr)
    result = result .. text
  end
  result = result .. " : "
  for i = 1, #t_consequence do
    local text = ts.get_node_text(t_consequence[i], bufnr)
    result = result .. text
  end

  local t = {}
  local lines = string.gmatch(result, "[^\r\n]+")
  for line in lines do
    table.insert(t, line)
  end

  -- replace the ternary with the swapped text
  local start_row, start_col = t_consequence[1]:start()
  local end_row, end_col = t_alternate[#t_alternate]:end_()
  vim.api.nvim_buf_set_text(bufnr, start_row, start_col, end_row, end_col, t)
end

return M
