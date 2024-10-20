--- https://github.com/nvim-telescope/telescope.nvim/blob/master/developers.md

local M = {}

local telescope = require("telescope")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values

function M.locate_file(opts)
  opts = opts or {}
  pickers
    .new(opts, {
      prompt_title = "Locate",
      finder = finders.new_oneshot_job(opts.cmd_list, opts),
      previewer = conf.file_previewer(opts),
      sorter = conf.file_sorter(opts),
    })
    :find()
end

-- TODO:filter out files that are not interesting
vim.api.nvim_create_user_command("Tlocate", function(info)
  local cmd_list = { "plocate" }
  local args = vim.split(info.args, "%s+")
  vim.list_extend(cmd_list, args)
  local opts = { cmd_list = cmd_list }
  M.locate_file(opts)
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
