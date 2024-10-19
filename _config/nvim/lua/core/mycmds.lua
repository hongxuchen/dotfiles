local u = require("core.utils")

local mycmd = vim.api.nvim_create_user_command

local dict_words = {}
mycmd("Dict", function(info)
  local word = info.args
  u.open_dict(word)
end, {
  nargs = "?",
  ---@diagnostic disable-next-line: unused-local
  complete = function(ArgLead, CmdLine, CursorPos)
    if #dict_words == 0 then
      -- print(string.format("arglead=%s, cmdline=%s, cursorpos=%d", ArgLead, CmdLine, CursorPos))
      local Path = require("plenary.path")
      local dict_fpath = u.en_dict
      ---@type Path
      local dict_file = Path:new(dict_fpath)
      if dict_file:is_file() then
        local data = dict_file:read()
        assert(data ~= nil and type(data) ~= "uv_fs_t")
        data = string.lower(data)
        dict_words = vim.split(data, "%s+", { plain = false, trimempty = true })
      else
        vim.notify("[Dict] " .. dict_fpath .. " not a file", vim.log.levels.ERROR, {})
      end
    end
    local candidates = {}
    local word_start = string.lower(ArgLead)
    for _, word in ipairs(dict_words) do
      if vim.startswith(word, word_start) then
        table.insert(candidates, word)
      end
    end
    return candidates
  end,
})

mycmd("ReloadCore", function()
  u.reload_core()
end, {})

mycmd("RunCMake", u.run_cmake, {})

mycmd("ToggleInlayHints", function()
  vim.lsp.inlay_hint.enable(true, { bufnr = 0 })
end, {})

-- it assumes no vim modeline exists
-- TODO: check with 'modelines' to set modeline at correct line (looser rule)
-- check whether a modeline alread exists and replace with our normalized one
local function add_modeline()
  local o = vim.bo
  local function get_modeline()
    local modeline_template = "vim: set ft=%s ts=%d sw=%d tw=%d:"
    local modeline = string.format(modeline_template, o.filetype, o.tabstop, o.shiftwidth, o.textwidth)
    return string.format(o.commentstring, modeline)
  end
  if o.commentstring == "" then
    vim.notify("no commentstring for ft=" .. o.filetype, vim.log.levels.WARN, {})
    return
  end
  if not vim.endswith(o.commentstring, "%s") then
    vim.notify("no-op for commentstring: " .. o.commentstring, vim.log.levels.WARN, {})
    return
  end
  local cs = o.commentstring:gsub("%s+%%s", "")
  local bufnr = vim.api.nvim_get_current_buf()
  local first_line = vim.api.nvim_buf_get_lines(bufnr, 0, 1, true)[1]
  if vim.startswith(first_line, cs) then
    local line_count = vim.api.nvim_buf_line_count(bufnr)
    local last_line = vim.api.nvim_buf_get_lines(bufnr, line_count - 1, -1, true)[1]
    if vim.startswith(last_line, cs) then
      vim.notify("no-ops when both start and end lines have comments", vim.log.levels.WARN, {})
      return
    end
    vim.api.nvim_buf_set_lines(bufnr, -1, -1, true, { "", get_modeline() })
  else
    vim.api.nvim_buf_set_lines(bufnr, 0, 0, true, { get_modeline(), "" })
  end
end

mycmd("AddModeline", add_modeline, {})

mycmd("VMore", function(info)
  local cmd = info.args
  local res = vim.api.nvim_exec2(cmd, { output = true })
  local lines = vim.split(res["output"], "\n", { plain = true, trimempty = true })
  local api = vim.api
  local newbuf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(newbuf, 0, -1, true, lines)
  vim.bo[newbuf].bufhidden = "wipe"
  vim.bo[newbuf].readonly = true
  vim.bo[newbuf].swapfile = false
  api.nvim_win_set_buf(0, newbuf)
  if info.bang then
    vim.bo[newbuf].filetype = "lua"
  else
    vim.bo[newbuf].filetype = "VMore"
    vim.bo[newbuf].buftype = "nofile"
  end
end, { nargs = "+", complete = "command", bang = true })
u.keymap("n", "<leader>I", ":VMore! =", { noremap = true }, "[vim] inspect complex lua object")
u.keymap("n", "<leader>i", ":=", { noremap = true }, "[vim] inspect lua object")
