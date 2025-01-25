local u = require("core.utils")

-- disable select-mode entries
u.au("VimEnter", {
  group = u.myAutoGroup,
  callback = function()
    vim.opt.selectmode = {}
    vim.keymap.set("x", "<C-g>", "<Nop>")
    vim.keymap.set("n", "gh", "<Nop>")
    vim.keymap.set("n", "gH", "<Nop>")
    vim.keymap.set("n", "g<C-H>", "<Nop>")
  end,
})

u.au("VimEnter", {
  group = u.myAutoGroup,
  callback = function(args)
    local should_skip = false
    -- or vim.fn.line2byte(vim.api.nvim_buf_line_count(args.buf)) ~= -1
    if vim.fn.argc() > 0 or not vim.o.modifiable then
      should_skip = true
    else
      for _, arg in pairs(vim.v.argv) do
        if arg == "-b" or arg == "-c" or vim.startswith(arg, "+") or arg == "-S" then
          should_skip = true
          break
        end
      end
    end
    if not should_skip then
      local status_ok, oil = pcall(require, "oil")
      if status_ok then
        -- oil.open(oil.get_current_dir())
        -- vim.cmd("Oil")
      else
        vim.cmd("Explore!")
      end
    end
  end,
})

-- resize splits if window got resized
u.au({ "VimResized" }, {
  group = u.myAutoGroup,
  callback = function()
    vim.cmd("tabdo wincmd =")
  end,
})

-- highlights updates when colorscheme changes
u.au("ColorScheme", {
  group = u.myAutoGroup,
  pattern = "*",
  callback = function()
    u.hi_statusline()
    vim.api.nvim_set_hl(0, "VertSplit", { link = "String" })
  end,
})

u.au("FileType", {
  group = u.myAutoGroup,
  pattern = { "gitcommit", "gitrebase", "gitconfig" },
  callback = function()
    vim.bo.bufhidden = "delete"
  end,
})

-- highlight on yank
u.au("TextYankPost", {
  group = u.myAutoGroup,
  pattern = "*",
  callback = function()
    vim.highlight.on_yank { higroup = "IncSearch", timeout = 100 }
  end,
})

-- revise afterwards
u.au({ "BufNewFile", "BufReadPost" }, {
  group = u.myAutoGroup,
  pattern = "*",
  callback = function(args)
    vim.wo.spell = false
    if vim.startswith(args.match, "fugitive:///") then
      vim.bo.bufhidden = "delete"
      u.keymap("n", "q", "<Cmd>q<CR>", u.buf_opts(args.buf), "[vim] quit window")
      return
    end
    if vim.bo.modifiable then
      if not vim.bo.readonly and vim.bo.fileencoding ~= "utf-8" then
        vim.notify("File not in UTF-8 format!", vim.log.levels.WARN)
      end
      -- don't continue comments
      vim.opt.formatoptions = vim.opt.formatoptions - "o"
      u.keymap("n", "<CR>", function()
        return string.format("m`%dO<ESC>``", vim.v.count1)
      end, u.buf_expr_opts(args.buf), "[vim] insert line above")
      u.keymap("n", "q", "<Nop>", u.buf_opts(args.buf), "[vim] nop for q")
    end
  end,
})
-- inside llvm project, add more as cpp filetype
u.au("BufEnter", {
  group = u.myAutoGroup,
  pattern = { "*.inc", "*.def" },
  callback = function(args)
    if string.find(args.match, "llvm", 1, true) then
      vim.bo[args.buf].filetype = "cpp"
    end
  end,
})

-- setup terminal buffer
u.au({ "TermOpen", "BufWinEnter", "WinEnter" }, {
  pattern = "term://*",
  callback = function(args)
    vim.cmd.startinsert()
    u.keymap("t", "<ESC>", "<C-\\><C-n>", u.opts, "[vim] use <ESC> for normal mode")
    u.keymap("t", "<C-v><ESC>", "<ESC>", u.opts, "[vim] remap <ESC>")
    u.keymap(
      "n",
      "<C-w><C-l>",
      "<Cmd>set scrollback=1<Bar>sleep 100m<Bar>set scrollback=10000",
      u.opts,
      "[vim] clear buffer in terminal"
    )
    if vim.bo[args.buf].filetype == "toggleterm" then
      u.keymap("n", "q", "<Cmd>ToggleTerm<CR>", u.buf_opts(args.buf), "[toggleterm] ToggleTerm")
    else
      u.keymap("n", "q", "<Cmd>bprevious<CR>", u.buf_opts(args.buf), "[vim] go to previous buffer")
    end
  end,
})

-- forbid simultaneous edits; make it default to readonly
u.au("SwapExists", {
  group = u.myAutoGroup,
  pattern = "*",
  callback = function()
    vim.v.swapchoice = "o"
    vim.notify("Found swap: " .. vim.v.swapname, vim.log.levels.WARN)
    vim.bo.modifiable = false
    vim.cmd.echohl("None")
  end,
})

-- Automatically update changed file in Vim
-- https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
-- https://vi.stackexchange.com/questions/14315/how-can-i-tell-if-im-in-the-command-window
u.au({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
  group = u.myAutoGroup,
  callback = function()
    if vim.api.nvim_get_mode().mode ~= "c" and not vim.fn.bufexists("[Command Line]") then
      vim.cmd("checktime")
    end
  end,
})

-- adjusted from https://stackoverflow.com/a/31581929/528929
---@type table<integer, table>
_G.my_saved_views = {}
local function save_buf_view()
  local bufnr = vim.api.nvim_get_current_buf()
  local v = vim.fn.winsaveview()
  my_saved_views[bufnr] = v
end
local function restore_buf_view()
  local bufnr = vim.api.nvim_get_current_buf()
  local saved = my_saved_views[bufnr]
  if saved then
    local v = vim.fn.winsaveview()
    local at_start_of_file = (v["lnum"] == 1 and v["col"] == 0)
    if not vim.wo.diff and at_start_of_file then
      vim.fn.winrestview(saved)
      my_saved_views[bufnr] = nil
    end
  end
end

-- restore buf winview
-- change directory w/o autochdir
u.au("BufEnter", {
  group = u.myAutoGroup,
  pattern = "*",
  callback = function()
    restore_buf_view()
    vim.cmd("silent! lcd %:p:h")
  end,
})

u.au("BufReadPost", {
  group = u.myAutoGroup,
  pattern = "*",
  callback = function(args)
    local uv = vim.uv
    local fpath = args.match
    local realpath = uv.fs_realpath(fpath)
    if realpath and realpath ~= fpath then
      vim.cmd("silent file " .. realpath)
    end
  end,
})

u.au("BufLeave", {
  group = u.myAutoGroup,
  pattern = "*",
  callback = function()
    save_buf_view()
  end,
})

---automatically set filetype based on file extension
---@param ext string
---@param ft string
local function set_ft_by_ext(ext, ft)
  u.au({ "BufNewFile", "BufReadPost" }, {
    group = u.myAutoGroup,
    pattern = "*." .. ext,
    callback = function()
      vim.bo.filetype = ft
    end,
  })
end

set_ft_by_ext("ll", "llvm")
