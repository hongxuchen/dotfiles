local fzf = require("fzf-lua")
local ations = fzf.actions

local mycmd = vim.api.nvim_create_user_command

--- ENV picker
local bpreviewer = require("fzf-lua.previewer.builtin")
local EnvPreviewer = bpreviewer.base:extend()

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

mycmd("Env", printenv, {})

-- zoxide picker TODO:

local list_zoxide = function(action, selected, o) end

-- git-file picker

local list_files_from_branch_action = function(action, selected, o)
  local file = fzf.path.entry_to_file(selected[1], o)
  local cmd = string.format("%s %s:%s", action, o.args, file.path)
  vim.cmd(cmd)
end

mycmd("ListFilesFromBranch", function(opts)
  fzf.files {
    cmd = "git ls-tree -r --name-only " .. opts.args,
    prompt = opts.args .. "> ",
    actions = {
      ["default"] = function(selected, o)
        list_files_from_branch_action("Gedit", selected, o)
      end,
      ["ctrl-s"] = function(selected, o)
        list_files_from_branch_action("Gsplit", selected, o)
      end,
      ["ctrl-v"] = function(selected, o)
        list_files_from_branch_action("Gvsplit", selected, o)
      end,
    },
    previewer = false,
    preview = {
      type = "cmd",
      fn = function(items)
        local file = fzf.path.entry_to_file(items[1])
        return string.format("git diff %s HEAD -- %s | delta", opts.args, file.path)
      end,
    },
  }
end, {
  nargs = 1,
  force = true,
  complete = function()
    local res = vim.system({ "git", "branch", "--all", "--sort=-committerdate" }, { text = true }):wait()
    if res.code == 0 then
      local branches = vim.split(res.stdout, "\n", { plain = true, trimempty = true })
      return vim.tbl_map(function(x)
        return x:match("[^%s%*]+"):gsub("^remotes/", "")
      end, branches)
    else
      return {}
    end
  end,
})

-- aerial picker
local function pick_document_symbols()
  local status_ok, aerial_fzf = pcall(require, "aerial.fzf")
  if not status_ok then
    vim.notify("aerial not installed, fallback to lsp_document_symbols", vim.log.levels.WARN)
    fzf.lsp_document_symbols()
    return
  end
  local aerial_labels = aerial_fzf.get_labels()
  require("aerial").setup {}
  local aerial_navigation = require("aerial.navigation")
  local aerial_data = require("aerial.data")
  local function goto_symbol(symbol)
    local idx = tonumber(symbol:match("^(%d+)"))
    for i, _, symbol_idx in aerial_data.get_or_create(0):iter { skip_hidden = true } do
      if idx == i then
        aerial_navigation.select {
          index = symbol_idx,
        }
        break
      end
    end
  end
  local opts = {
    prompt = ":",
    previewer = bpreviewer,
    hls = { cursorline = "" },
    winopts = {
      title = " Document Symbols ",
      title_pos = "center",
      height = 0.6,
      width = 0.4,
    },
    actions = {
      ["default"] = function(selected)
        goto_symbol(selected[1])
      end,
    },
  }
  fzf.fzf_exec(aerial_labels, opts)
end

mycmd("DocumentSymbols", pick_document_symbols, {})
