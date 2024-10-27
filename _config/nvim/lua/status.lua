local M = {}

local u = require("core.utils")

local MAX_LEN = 100
DOTS = ".."

local api = vim.api

u.hi_statusline()

---@param fpath string
---@return string
local function simplified_homepath(fpath)
  local homel = string.len(vim.env.HOME)
  if fpath:sub(1, homel) == vim.env.HOME then
    return "~" .. fpath:sub(homel + 1)
  else
    return fpath
  end
end

local function get_fpath_pair()
  local bufnr = api.nvim_get_current_buf()
  local fpath = api.nvim_buf_get_name(bufnr)
  local cwd = vim.uv.cwd()
  assert(cwd, "cwd should exist")
  ---@type string
  local fname
  if fpath == cwd then
    fname = "."
  elseif vim.startswith(fpath, cwd) then
    fname = fpath:sub(#cwd + 2)
  else
    fname = simplified_homepath(fpath)
  end

  cwd = simplified_homepath(cwd)
  local cwd_len = string.len(cwd)
  if cwd_len > MAX_LEN then
    cwd = DOTS .. string.sub(cwd, cwd_len - MAX_LEN + 1 + string.len(DOTS))
  end

  return cwd, fname
end

---@return string
local function diagnostic_status()
  local bufnr = api.nvim_win_get_buf(0)
  local err_num = #vim.diagnostic.get(bufnr, { severity = vim.diagnostic.severity.ERROR })
  local warn_num = #vim.diagnostic.get(bufnr, { severity = vim.diagnostic.severity.WARN })
  local msg_tbl = {}
  if err_num > 0 then
    table.insert(msg_tbl, u.signs.Error .. err_num)
  end
  if warn_num > 0 then
    table.insert(msg_tbl, u.signs.Warn .. warn_num)
  end
  return table.concat(msg_tbl, " ")
end

local function dap_status()
  local ok, dap = pcall(require, "dap")
  if not ok then
    return ""
  else
    return dap.status()
  end
end

local function indicator()
  local issues = {}
  if vim.o.fileformat ~= "unix" then
    table.insert(issues, vim.o.fileformat)
  end
  if vim.o.fileencoding ~= "utf-8" then
    table.insert(issues, vim.o.fileencoding)
  end
  local s = ""
  for _, v in ipairs(issues) do
    s = s .. "[" .. v .. "] "
  end
  return s
end

function M.statusline()
  local cwd, fname = get_fpath_pair()
  return table.concat {
    "%#StatusLeft#",
    "%<",
    fname,
    " %m%r%y%q",
    indicator(),
    "%#StatusLM#  ",
    diagnostic_status(),
    "  ",
    -- dap_status(),
    -- "  ",

    "%=",
    "%#StatusMid#",
    cwd,

    "%=",
    "%#StatusRight#",
    "%14(%l/%L:%c%)",
  }
end

return M
