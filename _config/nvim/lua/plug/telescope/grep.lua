local M = {}

local u = require("core.utils")

local t = require("telescope")
t.load_extension("live_grep_args") -- use rg with args

local lga_shortcuts = require("telescope-live-grep-args.shortcuts")
local lga_helpers = require("telescope-live-grep-args.helpers")

local grep_under_default_opts = {
  postfix = " -F ",
  quote = true,
  trim = true,
}

local function process_grep_under_text(value, opts)
  opts = opts or {}
  opts = vim.tbl_extend("force", grep_under_default_opts, opts)

  if opts.trim then
    value = vim.trim(value)
  end

  if opts.quote then
    value = lga_helpers.quote(value, opts)
  end

  if opts.postfix then
    value = value .. opts.postfix
  end

  return value
end

function M.grep_word_under_cursor(opts)
  local text = u.word_under_cursor(opts.big)
  text = process_grep_under_text(text, opts)
  local final_opts = vim.tbl_extend("force", { default_text = text }, opts or {})
  t.extensions.live_grep_args.live_grep_args(final_opts)
end

function M.grep_visual_selection(opts)
  local visual = lga_shortcuts.get_visual()
  local text = visual[1] or ""
  text = process_grep_under_text(text, opts)
  local final_opts = vim.tbl_extend("force", { default_text = text }, opts or {})
  t.extensions.live_grep_args.live_grep_args(final_opts)
end

return M
