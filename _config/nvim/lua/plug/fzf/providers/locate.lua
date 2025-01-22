local M = {}

local fzf = require("fzf-lua")
local actions = require("fzf-lua.actions")
local bpreviewer = require("fzf-lua.previewer.builtin")

function M.pick_locate(cmd_opts)
  local opts = {
    prompt = ":",
    previewer = bpreviewer,
    winopts = {
      title = " Fuzzy Locate Files ",
      title_pos = "center",
    },
    actions = {
      ["enter"] = actions.file_edit_or_qf,
    },
  }
  local cmd = table.concat(cmd_opts.cmd_list, " ")
  fzf.fzf_exec(cmd, opts)
end

-- TODO:filter out files that are not interesting
vim.api.nvim_create_user_command("Flocate", function(info)
  local cmd_list = { "plocate" }
  local args = vim.split(info.args, "%s+")
  vim.list_extend(cmd_list, args)
  local opts = { cmd_list = cmd_list }
  M.pick_locate(opts)
end, {
  nargs = "+",
  complete = function(ArgLead, CmdLine, CursorPos)
    local complete_opts = { "--limit ", "--regex", "-r", "--literal", "--basename", "-i" }
    return vim.tbl_filter(function(word)
      return vim.startswith(word, ArgLead)
    end, complete_opts)
  end,
})

return M
