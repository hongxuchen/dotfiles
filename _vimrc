" vim: set ft=vim ts=4 sw=2 tw=78 et :
" =====================================================
" AUTHOR: Hongxu Chen
" EMAIL:  leftcopy.chx@gmail.com
"
" =====================================================

" =====================================================
" dirty tricks
" =====================================================

" When editing a file, always jump to the last known cursor position.

autocmd BufNewFile,BufRead *.ipynb setfiletype javascript

" Set working directory
set autochdir
" save and reload vimrc
nnoremap <silent> <leader>v :w<CR>:source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" sudo write this
cnoremap W! w !sudo tee % >/dev/null

" Paste from clipboard
set clipboard=unnamed "Use system clipboard ("*)
nnoremap <leader>p :set paste! <CR>

" Remove trailing whitespace
nnoremap <leader>f :%s/\s\+$//<cr>:let @/=''<CR>

nnoremap <leader>s :pwd <CR>

" C-j to insert a newline
nnoremap <NL> i<CR><ESC>

" remap j,k
nnoremap j gj
nnoremap k gk
nnoremap gj j
noremap gk k

" don't use macro
nnoremap q :bN<CR>

" remap D to remove line without x register, anyway I have cc
nnoremap D "_dd
vnoremap D "_d

" history scrolls in Ex mode
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" used for git diff
map <silent> <leader>1 :diffget 1<CR> :diffupdate<CR>
map <silent> <leader>2 :diffget 2<CR> :diffupdate<CR>
map <silent> <leader>3 :diffget 3<CR> :diffupdate<CR>
map <silent> <leader>4 :diffget 4<CR> :diffupdate<CR>

" refresh if file in Vim is updated by external program,TODO
noremap <silent><F5> :checktime<CR>:exe ":echo 'file refreshed'"<CR>
inoremap <silent><F5> <C-O>:checktime<CR>:exe ":echo 'file refreshed'"<CR>

" visual-star-search, makes * and # work on visual mode too.
function! s:VSetSearch(cmdtype)
  let temp = @s
  norm! gv"sy
  let @/ = '\V' . substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = temp
endfunction
xnoremap * :<C-u>call <SID>VSetSearch('/')<CR>/<C-R>=@/<CR><CR>
xnoremap # :<C-u>call <SID>VSetSearch('?')<CR>?<C-R>=@/<CR><CR>

" recursively vimgrep for word under cursor or selection when hitting leader-star
nnoremap <leader>* :execute 'noautocmd vimgrep /\V' . substitute(escape(expand("<cword>"), '\'), '\n', '\\n', 'g') . '/ **'<CR>
vnoremap <leader>* :<C-u>call <SID>VSetSearch()<CR>:execute 'noautocmd vimgrep /' . @/ . '/ **'<CR>

" open help:info in new tab
cabbrev hh tab help

" =====================================================
" emacs like settings
" =====================================================
" both insert and normal
nnoremap <silent><C-x>0 :hide<CR>
inoremap <silent><C-x>0 <C-o>:hide<CR>
nnoremap <silent><C-x>1 :hide :only<CR>
inoremap <silent><C-x>1 <C-o>:hide :only<CR>
nnoremap <silent><C-x>k :bd<CR>
inoremap <silent><C-x>k <C-o>:bd<CR>
nnoremap <silent><C-x><C-s> :w<CR>:exe ":echo 'saved' bufname(\"%\")"<CR>
inoremap <silent><C-x><C-s> <C-o>:w<CR><C-o>:exe ":echo 'saved' bufname(\"%\")"<CR>
nnoremap <silent><C-x>s :wall<CR>:exe ":echo 'saved all buffers'"<CR>
inoremap <silent><C-x>s <C-o>:wall<CR>
nnoremap <silent><C-x>i :read<Space>
inoremap <silent><C-x>i <C-o>:read<Space>
nnoremap <silent><C-x><C-w> :write<Space>
inoremap <silent><C-x><C-w> <C-o>:write<Space>
nnoremap <silent><C-x><C-q> :set invreadonly<CR>
inoremap <silent><C-x><C-q> <C-o>:set invreadonly<CR>
nnoremap <silent><C-x><C-c> :wqall<CR>
inoremap <silent><C-x><C-c> <C-o>:wqall<CR>
nnoremap <silent><C-x><C-j> :E<CR>
inoremap <silent><C-x><C-J> <C-o>:E<CR>
nnoremap <silent><C-x>b :CtrlPMRUFiles<CR>
inoremap <silent><C-x>b <C-o>:CtrlPMRUFiles<CR>
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>0

inoremap <silent><C-f> <Right>
inoremap <silent><C-b> <Left>
inoremap <silent><C-d> <Del>
inoremap <silent><M-n> <C-o>:cnext<CR>
inoremap <silent><M-p> <C-o>:cprevious<CR>

cnoremap <C-g> <ESC><ESC>

" =====================================================
" general settings
" =====================================================
syntax on                     " syntax highlighing
filetype on                   " try to detect filetypes
filetype plugin on            " recognize filetype
filetype plugin indent on     " enable loading indent file for filetype
" set textwidth=80              "TODO
" set viminfo+=!               " global variables
set grepprg=grep\ -nH\ $*     "grep always displays filename
set ttyfast
set splitright                " 
set guioptions=               " simpler
set nrformats=                " treat all numberals as decimal
set tabpagemax=5              " only 5 tabs
set gdefault                  " substitute globally
set nocompatible              " Don't be compatible with vi
set colorcolumn=120
set tabpagemax=15             " max number of tabs
set number                    " Display line numbers
set numberwidth=1             " using only 1 column (and 1 space) while possible
set background=dark           " We are using dark background in vim
set title                     " show title in console title bar
set wildmenu                  " Menu completion in command mode on <Tab>
set wildmode=list:longest     " list and match longest
set wildmode=longest:full,list:full
set nocursorline              " have a line indicate the cursor location
set nostartofline             " Avoid moving cursor to BOL when jumping around
set virtualedit=block         " Let cursor move past the last char in <C-v> mode
set iskeyword+=$,@,%,#,-    " don't allow to be separated by line break
set foldmethod=manual         " TODO
set hidden                    " set buffer hidden
set whichwrap=b,s,h,l,<,>,[,]
set winaltkeys=no             " do not use ALT as menu trigger
set sel=exclusive
set scrolloff=5               " Keep 5 context lines above and below the cursor
set backspace=2               " Allow backspacing over autoindent, EOL, and BOL
set showmatch                 " Briefly jump to a paren once it's balanced
set wrap                      " wrap text
set linebreak                 " don't wrap textin the middle of a word
set autoindent                " always set autoindenting on
set smartindent               " use smart indent if there is no indent file
set tabstop=4                 " <tab> inserts 4 spaces
set shiftwidth=4              " but an indent level is 2 spaces wide.
set softtabstop=4             " <BS> over an autoindent deletes both spaces.
set expandtab                 " Use spaces, not tabs, for autoindent/tab key.
set shiftround                " rounds indent to a multiple of shiftwidth
set matchpairs+=<:>           " show matching <> (html mainly) as well
set foldmethod=syntax         " allow us to fold on indents
set foldlevel=99              " don't fold by default
set history=200               " up/down can also be used when some words have been inserted
" colorscheme delek           " koehler,elflord,murphy,torte,evening,delek
" Reading/Writing
set noautowrite               " Never write a file unless I request it.
set noautowriteall            " NEVER.
set autoread                  " automatically re-read changed files.
set modeline                  " Allow vim options to be embedded in files;
set modelines=5               " they must be within the first or last 5 lines.
set ffs=unix,dos,mac          " Try recognizing dos, unix, and mac line endings.
" Messages, Info, Status
set vb t_vb=                  " Disable all bells
set noerrorbells
set confirm                   " Y-N-C prompt if closing with unsaved changes.
set showcmd                   " Show incomplete normal mode commands as I type.
set report=0                  " : commands always print changed line count.
set shortmess+=atI            " Use [+]/[RO]/[w] for modified/readonly/written.
set laststatus=2              " Always show statusline, even if only 1 window.
set statusline=[%n]\ %<%.99f\ %h%w%m%r%{exists('*CapsLockStatusline')?CapsLockStatusline():''}%y%=%-16(\ %l,%c-%v\ %)%P
set listchars=tab:>-,eol:$,trail:-,precedes:<,extends:> " when list needed
set lcs=tab:>\ ,trail:_,precedes:<,extends:\
" encoding
set fileencodings=utf-8,ucs-bom,gbk,cp936,gb18030
set fileencoding=utf-8
set termencoding=utf-8
set encoding=utf-8
" searching and Patterns
set ignorecase                " Default to using case insensitive searches,
set smartcase                 " unless uppercase letters are used in the regex.
set smarttab                  " Handle tabs more intelligently
set hlsearch                  " Highlight searches by default.
set incsearch                 " Incrementally search while typing a /regex
" ignore these files when completing
set wildignore+=*/tmp/*,*.swp,*.zip     " MacOSX/Linux"
set wildignore+=*.o,*.obj,*.so,*.a,*.bc
set wildignore+=.git,*.elc
set wildignore+=eggs/**,*.egg-info/**,*.pyc,*.pyo,*pyd
set wildignore+=*.class,*.jar
set wildignore+=*.aux,*.toc,*.out
set wildignore+=*.bak,*.exe,*.chm,*.png,*.jpg,*.jpeg,*.gif,*.avi,*.rm,*.rmvb
" completion
" set completeopt=menu,longest
set completeopt=longest
set completeopt=longest,menuone
set pumheight=8

" =====================================================
" tool and plugin settings
" =====================================================
runtime ftplugin/man.vim
runtime macros/matchit.vim
" VUNDLE rtp, @see https://github.com/gmarik/vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'szw/vim-g'
Bundle 'kien/ctrlp.vim'
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-commentary'
" Bundle 'vim-ruby/vim-ruby'
" git syntax, indent, and filetypes
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rhubarb'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-tbone'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-jdaddy'
Bundle 'Tagbar'
Bundle 'jiangmiao/auto-pairs'
Bundle 'Superbil/llvm.vim'
Bundle 'Cpp11-Syntax-Support'
Bundle 'scrooloose/nerdtree'
Bundle 'nanotech/jellybeans.vim'
Bundle 'ack.vim'
Bundle 'tomasr/molokai'
Bundle 'endel/vim-github-colorscheme'
Bundle 'Valloric/YouCompleteMe'

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

" nerdtree
noremap <leader>T :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']
let g:NERDTreeDirArrows=0

" =====================================================
" filetype settings
" =====================================================
autocmd FileType lisp setl cms=;;%s
autocmd FileType cmake setl cms=#%s
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" append modeline
function! AppendModeline()
  let l:modeline = printf(" vim: set ft=%s ts=%d sw=%d tw=%d %set :",
        \ &filetype, &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("0"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

" let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_working_path_mode = 'ra'

autocmd BufRead,BufNewFile /usr/include/* set ft=cpp

" function! StartUp()
"     if 0 == argc()
"         NERDTree
"     end
" endfunction
" autocmd VimEnter * call StartUp()

colorscheme molokai

let g:ycm_confirm_extra_conf = 0
let g:ycm_add_preview_to_completeopt=1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_min_num_of_chars_for_completion = 99 
let g:ycm_min_num_identifier_candidate_chars = 2
let g:ycm_auto_trigger = 1
let g:ycm_collect_identifiers_from_tags_files = 0 " Let YCM read tags from Ctags file
let g:ycm_seed_identifiers_with_syntax = 1 " Completion for programming language's keyword
let g:ycm_complete_in_comments = 1 " Completion in comments
let g:ycm_complete_in_strings = 1 " Completion in string

"vim-g
let g:vim_g_f_command = "Gf"
let g:vim_g_command = "Go"
