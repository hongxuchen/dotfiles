vim.cmd.filetype("on")
vim.cmd.filetype { "plugin", "indent", "on" }

if vim.env.NVIM_CONFIG == nil then
  vim.env.NVIM_CONFIG = vim.fs.joinpath(vim.env.HOME, ".config/nvim")
end

local opt = vim.opt
local o = vim.o

-- not use ';'/',' for move repetition
vim.g.mapleader = ";" -- for general use
vim.g.maplocalleader = "," -- for dev use

-- read settings
-- opt.whichwrap = {"b","s","h","l","<",">","[","]"}
o.autochdir = false
o.autoindent = true
o.cedit = "<C-y>" -- default is <C-F>, conflicting with forward moving
o.compatible = false
o.encoding = "utf-8"
o.errorbells = false
o.expandtab = true
o.fileencoding = "utf-8"
o.foldenable = false
o.foldlevel = 99
o.foldmethod = "indent" -- using 'syntax' is quite SLOW
o.grepprg = "rg --vimgrep --no-heading -s"
-- o.guioptions = ""
o.hidden = true
o.history = 200
o.hlsearch = true
o.ignorecase = true -- works with smartcase
o.incsearch = true
o.list = true
o.matchtime = 2
o.modeline = true
o.modelines = 5
o.nrformats = ""
o.number = true
o.pumheight = 12
o.report = 99 -- add threshold to report less line changes
o.scrolloff = 0
o.selection = "inclusive"
o.shada = "!,'9999,<50,s10,h"
o.shiftround = true
o.shiftwidth = 0
o.showcmd = true
o.showmatch = true
o.showmode = false
o.smartcase = true
o.smartindent = true
o.smarttab = true
o.softtabstop = 4
o.splitbelow = true
o.splitkeep = "screen"
o.splitright = true
o.startofline = false
o.tabpagemax = 5
o.tabstop = 4
-- o.termencoding = "utf-8"
o.termguicolors = true
o.title = true
o.virtualedit = "block"
o.whichwrap = "b,s,h,l,<,>,[,]"
o.wildignorecase = true
o.wildmenu = true
o.winaltkeys = "no"
o.wrap = true
opt.backspace = { "indent", "eol", "start" }
opt.complete:remove { "t" }
opt.completeopt = { "longest" }
opt.ffs = { "unix", "dos", "mac" }
opt.fileencodings = { "utf-8", "ucs-bom", "gbk", "cp936", "gb18030" }
opt.iskeyword:append { "$", "#", "@", "-", "%" }
opt.listchars = { tab = "»⋅", trail = "-", precedes = "<", extends = ">" }
opt.matchpairs:append { "<:>" }
opt.wildmode = { "longest:full", "list:full" }
opt.wildoptions = { "pum", "tagfile" }
opt.wildignore = {
  "*.aux",
  "*.avi",
  "*.bak",
  "*.bc",
  "*.chm",
  "*.class",
  "*.egg-info/**",
  "*.elc",
  "*.exe",
  "*.gif",
  "*.jar",
  "*.jpeg",
  "*.jpg",
  "*.o",
  "*.obj",
  "*.out",
  "*.png",
  "*.pyc",
  "*.pyo",
  "*.rm",
  "*.rmvb",
  "*.so",
  "*.swap",
  "*.toc",
  "*.zip",
  "*pyd",
  ".git",
  "eggs/**",
}

-- write settings
-- o.showbreak = "+ "
-- o.tagcase = "match"
o.autoread = true
o.autowrite = false
o.autowrite = true
o.autowriteall = false
o.backup = false
o.breakindent = true
o.clipboard = "unnamed,unnamedplus"
o.colorcolumn = ""
o.confirm = true
o.dictionary = vim.fs.joinpath(vim.env.NVIM_CONFIG, "dicts/en.dict")
o.cursorline = true
o.formatoptions = "cjlnq"
o.inccommand = "split"
o.joinspaces = false
o.laststatus = 3
o.linebreak = true
o.mouse = nil
o.shortmess = "astWAIc"
o.showtabline = 2
o.signcolumn = "yes"
o.spellfile = vim.fs.joinpath(vim.env.NVIM_CONFIG, "spell/en.utf-8.add")
o.spell = false
o.timeout = true
o.timeoutlen = 500
o.ttyfast = true
o.updatecount = 500
o.updatetime = 200
o.writebackup = false
opt.cursorlineopt = { "number" }
opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }

local g = vim.g
-- disable standard plugins
-- g.loaded_netrw = 1
-- g.loaded_netrwPlugin = 1
g.loaded_2html_plugin = 1
g.loaded_getscriptPlugin = 1
g.loaded_logipat = 1
g.loaded_matchit = 1
g.loaded_rrhelper = 1
g.loaded_spellfile_plugin = 1
g.loaded_tutor_mode_plugin = 1
g.loaded_vimball = 1
g.loaded_vimballPlugin = 1

-- disable some providers
g.loaded_node_provider = 0
g.loaded_perl_provider = 0
g.loaded_python_provider = 0
g.loaded_ruby_provider = 0

vim.g.vimsyn_embed = "l"
vim.g.oil_detail = false

--- FIXME: for neovim, it doesn't work
-- g.netrw_localrmdir='rm -r'
