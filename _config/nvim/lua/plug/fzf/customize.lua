local fzf = require("fzf-lua")
local ations = fzf.actions

--- custom fzf pickers
local builtin = require("fzf-lua.previewer.builtin")
local EnvPreviewer = builtin.base:extend()

function EnvPreviewer:new(o, opts, fzf_win)
  EnvPreviewer.super.new(self, o, opts, fzf_win)
  setmetatable(self, EnvPreviewer)
  return self
end

function EnvPreviewer:populate_preview_buf(entry_str)
  local tmpbuf = self:get_tmp_buffer()
  local entry = vim.system({ "printenv", entry_str }, { text = true }):wait().stdout:gsub("[\n\r]", " ")
  vim.api.nvim_buf_set_lines(tmpbuf, 0, -1, false, {
    " " .. entry,
  })
  self:set_preview_buf(tmpbuf)
  -- self.win:update_scrollbar()
end

function EnvPreviewer:gen_winopts()
  local new_winopts = {
    wrap = true,
    number = false,
  }
  return vim.tbl_extend("force", self.winopts, new_winopts)
end

local function printenv()
  local cmd = "printenv | cut -d= -f1"
  local opts = {
    prompt = ":",
    previewer = EnvPreviewer,
    hls = { cursorline = "" },
    winopts = {
      title = " env variables ",
      title_pos = "center",
      height = 0.4,
      preview = {
        hidden = "nohidden",
        horizontal = "down:5%",
      },
    },
    actions = {
      ["default"] = function(selected)
        vim.notify(
          vim.system({ "printenv", selected[1] }, { text = true }):wait().stdout:gsub("[\n\r]", " "),
          vim.log.levels.INFO,
          { ft = "bash" }
        )
      end,
    },
  }
  fzf.fzf_exec(cmd, opts)
end

vim.api.nvim_create_user_command("Env", printenv, {})
