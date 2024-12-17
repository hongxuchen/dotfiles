local u = require("core.utils")

-- normal-mode
u.keymap("n", "Q", "q", u.opts, "[vim] use Q for recording")
u.keymap("n", "<C-u>", "<C-u>zz", u.opts, "[vim] half page up and center")
u.keymap("n", "<C-d>", "<C-d>zz", u.opts, "[vim] half page down and center")
u.keymap("n", "n", "nzzzv", u.opts, "[vim] search next, center, and unfold")
u.keymap("n", "N", "Nzzzv", u.opts, "[vim] search prev, center, and unfold")
u.keymap("n", "<ESC>", "<Cmd>nohl<Bar>echo<CR>", u.opts, "[vim] reset highlight search and errmsg")
-- taken from https://github.com/tpope/vim-unimpaired/blob/master/doc/unimpaired.txt
u.keymap("n", "[q", "<Cmd>cprevious<CR>", u.opts, "[vim] previous in quickfix list")
u.keymap("n", "]q", "<Cmd>cnext<CR>", u.opts, "[vim] next in quickfix list")
u.keymap("n", "[Q", "<Cmd>cfirst<CR>", u.opts, "[vim] first in quickfix list")
u.keymap("n", "]Q", "<Cmd>clast<CR>", u.opts, "[vim] last in quickfix list")
u.keymap("n", "[<C-q>", "<Cmd>cpfile<CR>", u.opts, "[vim] previous file in quickfix list")
u.keymap("n", "]<C-q>", "<Cmd>cnfile<CR>", u.opts, "[vim] next file in quickfix list")
u.keymap("n", "[l", "<Cmd>lprevious<CR>", u.opts, "[vim] previous in location list")
u.keymap("n", "]l", "<Cmd>lnext<CR>", u.opts, "[vim] next in location list")
u.keymap("n", "[L", "<Cmd>lfirst<CR>", u.opts, "[vim] first in location list")
u.keymap("n", "]L", "<Cmd>llast<CR>", u.opts, "[vim] last in location list")
u.keymap("n", "[<C-l>", "<Cmd>lpfile<CR>", u.opts, "[vim] previous file in location list")
u.keymap("n", "]<C-l>", "<Cmd>lnfile<CR>", u.opts, "[vim] next file in location list")
u.keymap("n", "<leader>c", ":tab terminal ", { noremap = true }, "[vim] run external command")
u.keymap("n", "<leader>qc", "<Cmd>cclose<CR>", u.opts, "[vim] close quickfix")
u.keymap("n", "<leader>ql", "<Cmd>lclose<CR>", u.opts, "[vim] close loclist")

--- insert-mode
u.keymap("i", "<C-a>", "<C-o>0", u.opts, "[vim] go to line start")
u.keymap("i", "<C-e>", "<C-o>$", u.opts, "[vim] go to line end")
u.keymap("i", "<C-f>", "<Right>", u.opts, "[vim] go forward")
u.keymap("i", "<C-b>", "<Left>", u.opts, "[vim] go backward")
u.keymap("i", "<C-d>", "<Del>", u.opts, "[vim] delete a character")
local undo_chars = { ",", ".", "!", "?", ";", ":" }
for _, c in ipairs(undo_chars) do
  vim.keymap.set("i", c, c .. "<C-g>u")
end

--- command-line mode
u.keymap("c", "<C-a>", "<Home>", u.term_opts, "[vim] go to beginning of line")
u.keymap("c", "<C-e>", "<End>", u.term_opts, "[vim] go to line end")
-- u.keymap("c", "<C-p>", "<Up>", u.term_opts, "[vim] line upwards")
-- u.keymap("c", "<C-n>", "<Down>", u.term_opts, "[vim] line downwise")
u.keymap("c", "<C-b>", "<Left>", u.term_opts, "[vim] go backward")
u.keymap("c", "<C-f>", "<Right>", u.term_opts, "[vim] go forward")
u.keymap("c", "<M-b>", "<S-Left>", u.term_opts, "[vim] word backward")
u.keymap("c", "<M-f>", "<S-Right>", u.term_opts, "[vim] word forward")
u.keymap("c", "<C-d>", "<Del>", u.term_opts, "[vim] delete a character")
u.keymap("c", "<C-g>", "<ESC><ESC>", u.term_opts, "[vim] switch to normal mode")

--- visual mode & select mode, note that we don't actually use select mode
u.keymap("v", "<", "<gv", u.opts, "[vim] indent left")
u.keymap("v", ">", ">gv", u.opts, "[vim] indent right")
u.keymap("v", "y", "ygv<ESC>", u.opts, "[vim] copy and not jump back")
u.keymap("v", "p", '"_c<ESC>p', u.opts, "[vim] paste w/o contaiminate registers")
