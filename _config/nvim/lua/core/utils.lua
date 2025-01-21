-- at the toplevel, no plugins should be required

local M = {}

assert(jit, "should use luajit")

M.os = jit.os

M.Lines = 99999
M.SizeInMegaBytes = 9
M.SizeInKiloBytes = 1024 * M.SizeInMegaBytes
M.SizeInBytes = 1024 * 1024 * M.SizeInMegaBytes

M.MaxWidth = 80
M.MaxHeight = 60
M.MinWidth = 30
M.MinHeight = 20

--- @type table<string, any>
M.opts = { noremap = true, silent = true }
M.expr_opts = { expr = true, silent = true }
M.buf_opts = function(bufnr)
  return vim.tbl_extend("force", M.opts, { buffer = bufnr })
end

M.buf_expr_opts = function(bufnr)
  return vim.tbl_extend("force", M.expr_opts, { buffer = bufnr })
end

--- @type table<string, any>
M.term_opts = { silent = false }

M.au = vim.api.nvim_create_autocmd
M.ag = vim.api.nvim_create_augroup

M.myAutoGroup = M.ag("myAutoGroup", {
  clear = true,
})

M.myWarnGroup = M.ag("myWarnGroup", {
  clear = true,
})

-- M.signs = { Error = "✘", Warn = "", Hint = "", Info = "" }
-- M.signs = { Error = "❌", Warn = "🚨", Hint = "", Info = "󰙎" }
M.signs = { Error = "", Warn = "", Hint = "", Info = "" }

local uv = vim.uv

M.not_win = uv.os_uname().sysname ~= "Windows_NT"

-- NOTE: this modifies the original value of opts it only adds desc (even in nil case), so no harm
---@param mode string | string[]
---@param lhs string
---@param rhs function | string
---@param opts table<string, any>
---@param desc string
---@return nil
function M.keymap(mode, lhs, rhs, opts, desc)
  if not desc or desc == "" then
    vim.keymap.set(mode, lhs, rhs, opts)
  else
    opts["desc"] = desc
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

M.en_dict = vim.o.dictionary

---@param big boolean?
---@return string
function M.word_under_cursor(big)
  ---@diagnostic disable-next-line: return-type-mismatch
  return big and vim.fn.expand("<cWORD>") or vim.fn.expand("<cword>")
end

---@return boolean
function M.is_floating_win()
  local wid = vim.api.nvim_get_current_win()
  return vim.api.nvim_win_get_config(wid).zindex
end

---@param str string
---@return string
function M.termcode(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

--- whether buf is big (`uv.fs_stat` only works for file)
---@param bufnr integer
---@return boolean
function M.is_bigfile(bufnr)
  local lines = vim.api.nvim_buf_line_count(bufnr)
  local size = vim.api.nvim_buf_get_offset(bufnr, lines)
  return size > M.SizeInBytes
end

---@param filename string
---@return table<string, any>
function M.read_json_file(filename)
  local Path = require("plenary.path")

  local path = Path:new(filename)
  if not path:exists() then
    return {}
  end

  local json_contents = path:read()
  local json = vim.json.decode(json_contents)
  return json or {}
end

function M.qf_populate(lines, mode)
  if mode == nil or type(mode) == "table" then
    lines = vim.tbl_map(function(item)
      return { filename = item, lnum = 1, col = 1, text = item }
    end, lines)
    mode = "r"
  end

  vim.fn.setqflist(lines, mode)

  vim.cmd([[ belowright cwindow | wincmd p ]])
end

---get the git directory
---@param root_pattern string[]
---@param cwd string
---@param select_submodule boolean? whether select submodule root, by default not
---@return string?
function M.get_pattern_root(root_pattern, cwd, select_submodule)
  local opts = {
    upward = true,
    path = cwd,
  }
  if not select_submodule then
    opts["type"] = "directory"
  end
  local matched = vim.fs.find(root_pattern, opts)
  return matched[1]
end

---@type string[]
M.root_patterns = { ".git" }
-- returns the root directory based on:
-- * lsp workspace folders
-- * lsp root_dir
-- * root pattern of filename of the current buffer
-- * root pattern of cwd
---@param select_submodule boolean?
---@return string
function M.get_workspace_root(select_submodule)
  ---@type string?
  local path = vim.api.nvim_buf_get_name(0)
  path = path ~= "" and uv.fs_realpath(path) or nil
  ---@type string[]
  local roots = {}
  if path then
    for _, client in pairs(vim.lsp.get_clients { bufnr = 0 }) do
      local workspace = client.config.workspace_folders
      local paths = workspace and vim.tbl_map(function(ws)
        return vim.uri_to_fname(ws.uri)
      end, workspace) or client.config.root_dir and { client.config.root_dir } or {}
      for _, p in ipairs(paths) do
        local r = uv.fs_realpath(p)
        if r == nil then
          vim.notify(string.format("cannot get root_dir for client=%s", client.name), vim.log.levels.WARN, {})
        elseif path:find(r, 1, true) then
          roots[#roots + 1] = r
        else
          vim.notify(string.format("%s not startswith %s", path, r), vim.log.levels.WARN, {})
        end
      end
    end
  end
  table.sort(roots, function(a, b)
    return #a > #b
  end)
  ---@type string?
  local root = roots[1]
  if not root then
    path = path and vim.fs.dirname(path) or uv.cwd()
    root = M.get_pattern_root(M.root_patterns, path, select_submodule)
    root = root and vim.fs.dirname(root) or uv.cwd()
  end
  ---@cast root string
  return root
end

--- run system command and return a list
---@param cmd string
---@return nil
function M.System(cmd)
  local cmd_list = vim.split(cmd, "%s", { plain = false, trimempty = true })
  local ret = vim.system(cmd_list, { text = true }):wait()
  if ret.signal ~= 0 then
    vim.notify(string.format("signal=%d\n%s", ret.signal, ret.stderr), vim.log.levels.ERROR, {})
    return
  end
  if ret.signal ~= 0 or ret.code ~= 0 then
    vim.notify(string.format("error=%d\n%s", ret.code, ret.stderr), vim.log.levels.WARN, {})
    return
  end
  assert(ret.stdout ~= "")
  if ret.stdout ~= "" then
    vim.print(ret.stdout)
  else
    vim.notify("successfully exit w/o stdout", vim.log.levels.INFO, {})
  end
end

function M.has_words_before()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

---return a ranged value
---@param v number
---@param min number
---@param max number
---@return number
local ranged = function(v, min, max)
  return math.max(min, math.min(max, v))
end

---centered opts
---@param winid integer
---@return table<string, any>
local function centered_opts(winid)
  local api = vim.api
  local width = api.nvim_win_get_width(winid)
  local win_width = ranged(math.ceil(width * 0.8 - 4), M.MinWidth, M.MaxWidth)
  local col = (width - win_width) / 2
  local height = api.nvim_win_get_height(winid)
  local win_height = ranged(math.ceil(height * 0.8 - 2), M.MinHeight, M.MaxHeight)
  local row = (height - win_height) / 2
  return {
    style = "minimal",
    border = "single",
    relative = "editor",
    width = win_width,
    height = win_height,
    row = row,
    col = col,
  }
end

---return opts below cursor (row, col)
---@param cur_win integer
---@param row integer
---@param col integer
---@param line_len integer
---@return table<string, any>
local function cursor_opts(cur_win, row, col, line_len)
  local api = vim.api
  local width = api.nvim_win_get_width(cur_win)
  local height = api.nvim_win_get_height(cur_win)

  local win_width = ranged(math.ceil(width * 0.8 - 4), M.MinWidth, M.MaxWidth)
  if width - col + 1 <= win_width then
    col = width - win_width
  end

  local win_height = math.min(line_len, M.MaxHeight)
  local bottom = height - row
  if bottom <= win_height then
    win_height = bottom
  end

  return {
    style = "minimal",
    border = "single",
    relative = "editor",
    width = win_width,
    height = win_height,
    row = row + 1,
    col = col,
  }
end

-- FIXME: "height" may not be positive; keymap not working
---@param text string?
function M.open_dict(text)
  local api = vim.api

  local from_cursor = false
  if not text or vim.trim(text) == "" then
    text = M.word_under_cursor()
    from_cursor = true
  end

  local win_buf = api.nvim_create_buf(false, true)
  vim.bo[win_buf].bufhidden = "wipe"
  vim.bo[win_buf].buftype = "nofile"
  vim.bo[win_buf].filetype = "my_dict"

  --- @type integer
  local cur_win = api.nvim_get_current_win()

  local dict_name = "dict"
  if vim.fn.executable(dict_name) ~= 1 then
    vim.notify("cannot find executable: " .. dict_name, vim.log.levels.WARN, {})
    return
  end

  local cmd_list = { dict_name, text }
  vim.system(
    cmd_list,
    { text = true },
    vim.schedule_wrap(function(ret)
      local lines
      if ret.signal ~= 0 or ret.code ~= 0 then
        lines = vim.split(ret.stderr, "\n", { plain = true })
      else
        lines = vim.split(ret.stdout, "\n", { plain = true })
      end
      api.nvim_buf_set_lines(win_buf, 0, -1, true, lines)

      local win_id
      local win_line = vim.fn.winline()
      local win_col = vim.fn.wincol()
      if from_cursor and win_line and win_col then
        local opts = cursor_opts(cur_win, win_line, win_col, #lines)
        win_id = api.nvim_open_win(win_buf, true, opts)
      else
        local opts = centered_opts(cur_win)
        win_id = api.nvim_open_win(win_buf, true, opts)
      end
      vim.keymap.set("n", "q", function()
        vim.api.nvim_win_close(win_id, true)
      end, { buffer = win_buf })
    end)
  )
end

function M.reload_core()
  local reloaded = {}
  for name, _ in pairs(package.loaded) do
    if name:match("^core") then
      package.loaded[name] = nil
      require(name)
      table.insert(reloaded, name)
    end
  end
  vim.notify(string.format("reloaded: %s", vim.inspect(reloaded)), vim.log.levels.INFO, {})
end

function M.hi_statusline()
  local status_bg = "#32344a"
  vim.api.nvim_set_hl(0, "StatusLeft", { fg = "yellow", bg = status_bg })
  vim.api.nvim_set_hl(0, "StatusLM", { fg = "lightred", bg = status_bg })
  vim.api.nvim_set_hl(0, "StatusMid", { fg = "lightblue", bg = status_bg })
  vim.api.nvim_set_hl(0, "StatusRight", { fg = "white", bg = status_bg })
end

function M.run_cmake()
  local root = M.get_workspace_root(false)
  local cmd_list = { "cmake", "--build", "build" }
  vim.system(
    cmd_list,
    { text = true, cwd = root },
    vim.schedule_wrap(function(ret)
      local lines
      if ret.signal ~= 0 or ret.code ~= 0 then
        vim.notify(ret.stderr, vim.log.levels.WARN, {})
      else
        vim.notify(ret.stdout, vim.log.levels.INFO, {})
      end
    end)
  )
end

function M.display_lua_obj(res)
  local content = vim.inspect(res)
  local lines = vim.split(content, "\n", { trimempty = true, plain = true })
  vim.cmd("botright vnew")
  local new_bufnr = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(new_bufnr, 0, -1, true, lines)
  vim.bo[new_bufnr].buftype = "nofile"
  vim.bo[new_bufnr].filetype = "lua"
  vim.bo[new_bufnr].bufhidden = "wipe"
end

-- filter using predicate f
function M.filter(tbl, f, keep_keys)
  if not tbl or tbl == {} then
    return {}
  end

  local t = {}

  local insert = function(key, value)
    if keep_keys then
      t[key] = value
    else
      table.insert(t, value)
    end
  end

  for key, value in pairs(tbl) do
    if f(key, value) then
      insert(key, value)
    end
  end

  return t
end

---whether the fpath is owned by me
---@param fpath string
---@return boolean
function M.file_owned_by_me(fpath)
  local fs_stat = uv.fs_stat(fpath)
  return fs_stat ~= nil and uv.getuid() == fs_stat.uid
end

---@param s string
---@return string
function M.stripped_newline(s)
  ---@diagnostic disable-next-line: redundant-return-value
  return s:gsub("\n[^\n]*$", "")
end

-- currying functions, fn(params) will be called on partial()
function M.partial(fn, ...)
  local n, args = select("#", ...), { ... }
  return function()
    return fn(unpack(args, 1, n))
  end
end

-- is buffer horizontally truncated
function M.is_htruncated(width, global)
  local current_width = (global and vim.api.nvim_get_option_value("columns", { scope = "global" }))
    or vim.api.nvim_win_get_width(0)
  return current_width <= width
end

-- is buffer vertical truncated
function M.is_vtruncated(height, global)
  local current_height = (global and vim.api.nvim_get_option_value("lines", { scope = "global" }))
    or vim.api.nvim_win_get_height(0)
  return current_height <= height
end

-- whether the value is in list
function M.is_in_list(value, list)
  for _, v in pairs(list) do
    if v == value then
      return true
    end
  end
  return false
end

-- get git repo root dir (or nil)
function M.get_git_root()
  local git_cmd = "git -C " .. uv.cwd() .. " rev-parse --show-toplevel"
  local git_run = vim.system(vim.split(git_cmd, " ", { plain = true }), { text = true })
  local ret = git_run:wait()
  if ret.code == 0 then
    return M.stripped_newline(ret.stdout)
  else
    return nil
  end
end

--- get winnrs for qflists visible in current tab
function M.get_visible_qflists()
  return M.filter(vim.api.nvim_tabpage_list_wins(0), function(_, winnr)
    return vim.fn.getwininfo(winnr)[1].quickfix == 1
  end)
end

-- TODO: MANUAL add support for gitlab, bitbucket etc as needed
function M.open_repo_on_github(remote)
  if M.get_git_root() == nil then
    vim.notify("not in a git repository", vim.log.levels.ERROR)
    return
  end

  local get_url_cmd = { "git", "config", "remote." .. remote .. ".url" }
  local get_url_run = vim.system(get_url_cmd, { text = true })
  local ret = get_url_run:wait()
  if ret.code ~= 0 or ret.signal ~= 0 then
    vim.notify(string.format("found invalid remote: [%s]", remote), vim.log.levels.ERROR)
    return
  end

  local url = M.stripped_newline(ret.stdout)

  assert(url, "could not get remote urls")
  url = url:gsub("git:", "https://")
  url = url:gsub("git@", "https://")
  url = url:gsub("com:", "com/")
  local open_cmd = { "open", "-u", url }
  local open_run = vim.system(open_cmd, { text = true })
  ret = open_run:wait()
  if ret.code == 0 then
    vim.notify(string.format("[%s] -> %s", remote, url), vim.log.levels.INFO)
  else
    vim.notify(string.format("open %s failed: %s", url, ret.stderr), vim.log.levels.ERROR)
  end
end

--- @param path string
--- @param mode string
M.read_file = function(path, mode)
  local fd = io.open(path, mode or "r")
  if not fd then
    return ""
  end
  local content = fd:read("*a")
  fd:close()
  return content or ""
end

-- mkdir for file
local function fs_file_mkdirs(path)
  local parents = {}
  vim.iter(vim.fs.parents(path)):all(function(dir)
    local fs_stat = vim.uv.fs_stat(dir)
    if not fs_stat then
      parents[#parents + 1] = dir
      return true
    end
    return false
  end)
  vim.iter(parents):rev():each(function(p)
    return vim.uv.fs_mkdir(p, 493)
  end)
end

-- path should be normalized
M.write_file = function(path, content, flag)
  if not vim.uv.fs_stat(path) then
    fs_file_mkdirs(path)
  end
  local fd = io.open(path, flag or "w")
  if not fd then
    return false
  end
  if content then
    fd:write(content)
  end
  fd:close()
  return true
end

return M
