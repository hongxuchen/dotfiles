" vim: set ft=vim ts=4 sw=2 tw=78 et :
" PRINCIPLES:
" plugins should be loaded as lazy as possible
" startup should be as fast as possible

source ~/.vread

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" save and reload ~/.vimrc
nnoremap <silent> <leader>v :w<CR>:source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>
vnoremap <leader>vs y:@"<CR>
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set clipboard^=unnamed,unnamedplus
nnoremap <leader>p :set paste! <CR>
" C-j to insert a newline
nnoremap <NL> i<CR><ESC>
" q for next buffer
nnoremap q :bN<CR>
" remap D to remove line without x register, anyway I have cc
nnoremap D "_dd
vnoremap D "_d
" refresh if file in Vim is updated by external program,TODO
noremap <silent><F5> :checktime<CR>:exe ":echo 'file refreshed'"<CR>
inoremap <silent><F5> <C-O>:checktime<CR>:exe ":echo 'file refreshed'"<CR>
" substitute word under cursor with ...
nnoremap <Leader>S :%s/\<<C-r><C-w>\>/

" emacs like settings(insert mode)
inoremap <silent><C-x>0 <C-o>:hide<CR>
inoremap <silent><C-x>1 <C-o>:hide :only<CR>
inoremap <silent><C-x>k <C-o>:bd<CR>
inoremap <silent><C-x><C-s> <C-o>:w<CR><C-o>:exe ":echo 'saved' bufname(\"%\")"<CR>
inoremap <silent><C-x>s <C-o>:wall<CR>
inoremap <silent><C-x>i <C-o>:read<Space>
inoremap <silent><C-x><C-w> <C-o>:write<Space>
inoremap <silent><C-x><C-q> <C-o>:set invreadonly<CR>
inoremap <silent><C-x><C-c> <C-o>:wqall<CR>
inoremap <silent><C-x><C-J> <C-o>:e.<CR>
inoremap <silent><C-e> <C-o>$
inoremap <silent><C-a> <C-o>0
inoremap <silent><C-f> <Right>
inoremap <silent><C-b> <Left>
inoremap <silent><C-d> <Del>
inoremap <silent><M-n> <C-o>:cnext<CR>
inoremap <silent><M-p> <C-o>:cprevious<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" append modeline
function! AppendModeline()
  let l:modeline = printf(" vim: set ft=%s ts=%d sw=%d tw=%d %set :",
        \ &filetype, &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("0"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

set ttyfast
set colorcolumn=120
set autochdir
set noautowrite               " Never write a file unless I request it.
set noautowriteall            " NEVER.
set autoread                  " automatically re-read changed files.
set confirm                   " Y-N-C prompt if closing with unsaved changes.
set laststatus=2              " Always show statusline, even if only 1 window.
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%=%-16(\ %l,%c-%v\ %)%P

autocmd FileType c,cpp,java,markdown autocmd BufWritePre <buffer> :%s/\s\+$//e
autocmd FileType html,eruby,rb,css,js,xml runtime! macros/matchit.vim

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/bundle')
Plug 'Superbil/llvm.vim', {'for': 'llvm'}
Plug 'kelwin/vim-smali', {'for': 'smali'}
Plug 'cespare/vim-toml', {'for': 'toml'}
"vim-g
Plug 'szw/vim-g'
let g:vim_g_f_command = "Gf"
let g:vim_g_command = "Go"

" asyncrun
Plug 'skywind3000/asyncrun.vim'

Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'plasticboy/vim-markdown'

Plug 'jiangmiao/auto-pairs'
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<M-b>'
" let g:AutoPairs = {'(':')', '[':']', '{':'}', '<':'>', "'":"'",'"':'"', '`':'`'}

Plug 'tell-k/vim-autopep8', {'for': 'python'}

Plug 'tpope/vim-commentary'
autocmd FileType cmake setl cms=#%s
autocmd FileType gdb setl cms=#%s
autocmd FileType c setl cms=//%s
autocmd FileType cpp setl cms=//%s
autocmd FileType lisp setl cms=;;%s
autocmd FileType scala setl cms=//%s
autocmd FileType tablegen setl cms=//%s
autocmd FileType unix setl cms=#%s
autocmd FileType xdefaults setl cms=!%s
set formatoptions=tq

Plug 'majutsushi/tagbar'
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" tagbar
noremap <leader>t :TagbarToggle<CR>
let g:tagbar_autoclose=0
let g:tagbar_left = 0
let g:tagbar_width = 31
let g:tagbar_autofocus = 0
let g:tagbar_sort = 1
let g:tagbar_compact = 1
let g:tagbar_expand = 0
let g:tagbar_singleclick = 0
let g:tagbar_foldlevel = 5
let g:tagbar_autoshowtag = 0
let g:tagbar_updateonsave_maxlines = 10000
let g:tagbar_systemenc = 'encoding'
let g:tagbar_type_rust = {
      \ 'ctagstype' : 'rust',
      \ 'kinds' : [
      \'T:types,type definitions',
      \'f:functions,function definitions',
      \'g:enum,enumeration names',
      \'s:structure names',
      \'m:modules,module names',
      \'c:consts,static constants',
      \'t:traits,traits',
      \'i:impls,trait implementations',
      \]
      \}


Plug 'Shougo/echodoc.vim'
let g:echodoc_enable_at_startup = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all'  }
Plug 'junegunn/fzf.vim'

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})
inoremap <expr> <c-x><c-k> fzf#complete('cat /usr/share/dict/words')

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Plug 'roxma/nvim-completion-manager'

Plug 'ervandew/supertab'

Plug 'vim-utils/vim-man'

" rust -- rls
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
let g:LanguageClient_autoStart = 1

Plug 'rust-lang/rust.vim'
let g:LanguageClient_serverCommands = {
      \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
      \}

Plug 'PlanStylite/nvim-cargo'

if g:os == "Linux"
Plug 'SirVer/ultisnips'
endif
Plug 'honza/vim-snippets'
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

Plug 'w0rp/ale'
let g:ale_sign_column_always = 1
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '--'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
" let g:ale_open_list = 1
" let g:ale_keep_list_window_open = 1
"

Plug 'critiqjo/lldb.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'airodactyl/neovim-ranger'
Plug 'panickbr/neovim-ranger'
Plug 'justinmk/vim-sneak'

Plug 'hkupty/iron.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'mattn/webapi-vim'
Plug 'euclio/vim-markdown-composer'

" color scheme
Plug 'endel/vim-github-colorscheme'
Plug 'altercation/vim-colors-solarized'
Plug 'tomasr/molokai'
Plug 'NLKNguyen/papercolor-theme'
Plug 'reedes/vim-colors-pencil'
Plug 'd11wtq/macvim256.vim'
Plug 'chmllr/elrodeo-vim-colorscheme'
Plug 'notpratheek/vim-sol'
Plug 'w0ng/vim-hybrid'
Plug 'kristijanhusak/vim-hybrid-material'
Plug 'penicolas/simplon.vim'
Plug 'nowk/genericdc'
Plug 'stulzer/heroku-colorscheme'
Plug 'atelierbram/vim-colors_atelier-schemes'
Plug 'mkarmona/materialbox'

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if g:os == "Darwin"
  set background=light
  " colorscheme PaperColor
  " colorscheme solarized
  colorscheme macvim256
else
  set background=light
  colorscheme Atelier_SulphurpoolLight
  " colorscheme PaperColor
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" netrw
" @see also https://gist.github.com/t-mart/610795fcf7998559ea80
" let g:netrw_list_hide='\(^\|\s\s\)\zs\.\S\+'
" let g:netrw_hide=1              " hide hidden files
" let g:netrw_dirhistmax=100      " keep more history
" let g:netrw_altfile = 1         " last edited file '#'
" let g:netrw_banner = 0            " no banner
" let g:netrw_browse_split = 4      " open in previous window
" let g:netrw_liststyle = 1         " thin view
" " let g:netrw_alto = 0            " open files on right
" let g:netrw_altv = 1              " open files on right
" let g:netrw_winsize = 12          " preview size
" let g:netrw_use_errorwindow=0     " suppress error window
" let g:netrw_preview=1             " open previews vertically
" function! VExplorerToggle()
"   if exists("t:expl_buf_num")
"     let expl_win_num = bufwinnr(t:expl_buf_num)
"     if expl_win_num != -1
"       let cur_win_nr = winnr()
"       exec expl_win_num . 'wincmd w'
"       close
"       exec cur_win_nr . 'wincmd w'
"       unlet t:expl_buf_num
"     else
"       unlet t:expl_buf_num
"     endif
"   else
"     exec '1wincmd w'
"     Vexplore
"     let t:expl_buf_num = bufnr("%")
"   endif
" endfunction
" noremap <leader>T :call VExplorerToggle()<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
