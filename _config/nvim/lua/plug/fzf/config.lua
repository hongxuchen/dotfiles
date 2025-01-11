local ok, fzf = pcall(require, "fzf-lua")
if not ok then
  return
end

local symbols_exclude = { "Variable", "String", "Number", "Text", "Boolean" }

fzf.setup {
  { "telescope", "fzf-native" },
  winopts = {
    height = 0.95,
    width = 0.95,
    row = 0.5,
    col = 0.5,
    boder = "none",
  },
}

local u = require("core.utils")

vim.keymap.set({ "n", "v", "i" }, "<C-x><C-f>", function()
  require("fzf-lua").complete_path()
end, { silent = true, desc = "[fzf] Fuzzy complete path" })

u.keymap("n", "<leader>f", function()
  fzf.files()
end, u.opts, "[fzf] find files in cur dir")

u.keymap("n", "<leader>ww", function()
  fzf.files {
    cwd = u.get_workspace_root(),
  }
end, u.opts, "[fzf] find files in workspace")

u.keymap("n", "<leader>m", function()
  fzf.manpages {}
end, u.opts, "[fzf] search manpages")

u.keymap("n", "<leader>r", fzf.resume, u.opts, "[fzf] resume last picker")

u.keymap("n", "<leader>g", fzf.git_status, u.opts, "[fzf] git status")
u.keymap("n", "<leader>h", fzf.helptags, u.opts, "[fzf] search help tags")
u.keymap("n", "<leader>o", fzf.oldfiles, u.opts, "[fzf] find old files")
u.keymap("n", "<leader>bb", function()
  fzf.buffers {}
end, u.opts, "[fzf] search buffers")

u.keymap("n", '<leader>"', fzf.registers, u.opts, "[fzf] show registers content")

u.keymap("n", "<leader><leader>", function()
  fzf.builtin {}
end, u.opts, "[fzf] run builtin pickers")

vim.keymap.set({ "n" }, "<leader>gB", function()
  if require("core.utils").get_git_root() ~= nil then
    fzf.git_branches()
  else
    vim.notify("not a git repository", vim.log.levels.WARN)
  end
end, { desc = "fzf git branches" })
vim.keymap.set({ "n" }, "<C-m>", function()
  vim.ui.input({ prompt = "search symbol: " }, function(sym)
    if not sym or sym == "" then
      return
    end
    fzf.lsp_workspace_symbols { lsp_query = sym }
  end)
end, { desc = "fzf workspace symbols" })
vim.keymap.set({ "n" }, "gm", function()
  fzf.lsp_document_symbols()
end, { desc = "fzf document symbols" })
vim.api.nvim_create_user_command("Autocmd", function()
  fzf.autocmds()
end, { desc = "fzf autocmds list" })
vim.api.nvim_create_user_command("Maps", function()
  fzf.keymaps()
end, { desc = "fzf maps list" })
vim.api.nvim_create_user_command("Highlights", function()
  fzf.highlights()
end, { desc = "fzf highlights list" })

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
  self.win:update_scrollbar()
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
