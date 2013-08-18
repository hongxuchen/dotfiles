" vim: set ft=vim ts=4 sw=2 tw=78 et :
" =====================================================
" AUTHOR: Hongxu Chen
" EMAIL:  leftcopy.chx@gmail.com
" =====================================================

" =====================================================
" dirty tricks
" =====================================================

" When editing a file, always jump to the last known cursor position.
" autocmd BufReadPost *
"       if line("'\"") > 1 && line("'\"") <= line("$") |
"         exe "normal! g`\"" |
"       endif

" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>
" save and reload vimrc
nnoremap <silent> <leader>v :w<CR>:source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" sudo write this
cnoremap W! w !sudo tee % >/dev/null


"read pdf files using pdftotext; used with :tabnew!
:command! -complete=file -nargs=1 Rpdf :r !pdftotext -nopgbrk <q-args> - |fmt -csw78

" Paste from clipboard
set clipboard=unnamed "Use system clipboard ("*)

" Remove trailing whitespace
nnoremap <leader>s :%s/\s\+$//<cr>:let @/=''<CR>

" auto-save whenever leaving insert mode
autocmd  InsertLeave *  write

" C-j to insert a newline
nnoremap <NL> i<CR><ESC>

" remap j,k
nnoremap j gj
nnoremap k gk
nnoremap gj j
noremap gk k

" don't use macro
nnoremap q :bN

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

" navigate faster by remap SPC/BS
nnoremap <space> 10jzz
nnoremap <backspace> 10kzz

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
nnoremap <C-x>0 :hide<CR>
inoremap <C-x>0 <C-o>:hide<CR>
nnoremap <C-x>1 :hide :only<CR>
inoremap <C-x>1 <C-o>:hide :only<CR>
nnoremap <C-x>k :bd<CR>
inoremap <C-x>k <C-o>:bd<CR>
nnoremap <C-x><C-s> :w<CR>
inoremap <C-x><C-s> <C-o>:w<CR>
" FIXME seems wrong
nnoremap <C-x>s :wall<CR>
inoremap <C-x>s <C-o>:wall<CR>
nnoremap <C-x>i :read<Space>
inoremap <C-x>i <C-o>:read<Space>
nnoremap <C-x><C-w> :write<Space>
inoremap <C-x><C-w> <C-o>:write<Space>
nnoremap <C-x><C-q> :set invreadonly<CR>
inoremap <C-x><C-q> <C-o>:set invreadonly<CR>
nnoremap <C-x><C-c> :qall<CR>
inoremap <C-x><C-c> <C-o>:qall<CR>
nnoremap <C-x>d :E<CR>
inoremap <C-x>d <C-o>:E<CR>
nnoremap <C-x>b :CtrlPMRUFiles<CR>
inoremap <C-x>b <C-o>:CtrlPMRUFiles<CR>

inoremap <silent><C-f> <Right>
inoremap <silent><C-b> <Left>
inoremap <C-d> <Del>
inoremap <M-n> <C-o>:cnext<CR>
inoremap <M-p> <C-o>:cprevious<CR>

cnoremap <C-g> <ESC><ESC>

" =====================================================
" general settings
" =====================================================
syntax on                     " syntax highlighing
filetype on                   " try to detect filetypes
filetype plugin on            " recognize filetype
filetype plugin indent on     " enable loading indent file for filetype
"set textwidth=80              " TODO
set viminfo+=!                " global variables
set grepprg=grep\ -nH\ $*     "grep always displays filename
set ttyfast
set splitright                " 
set guioptions=               " simpler
set nrformats=                " treat all numberals as decimal
set tabpagemax=5              " only 5 tabs
set gdefault                  " substitute globally
set nocompatible              " Don't be compatible with vi
set tabpagemax=15             " max number of tabs
set number                    " Display line numbers
set numberwidth=1             " using only 1 column (and 1 space) while possible
set background=dark           " We are using dark background in vim
set title                     " show title in console title bar
set wildmenu                  " Menu completion in command mode on <Tab>
set wildmode=list:longest     " list and match longest
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
colorscheme default           " koehler,elflord,murphy,torte,evening,delek
" colorscheme delek
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
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})
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
set wildignore+=*.o,*.obj,*.so,*.a,*.bc
set wildignore+=.git,*.elc
set wildignore+=eggs/**,*.egg-info/**,*.pyc,*.pyo,*pyd
set wildignore+=*.class,*.jar
set wildignore+=*.aux,*.toc,*.out
set wildignore+=*.bak,*.exe,*.chm,*.png,*.jpg,*.jpeg,*.gif,*.avi,*.rm,*.rmvb
" completion
set completeopt=menu,longest
set pumheight=8
let g:acp_completeoptPreview=1

" =====================================================
" tool and plugin settings
" =====================================================
runtime ftplugin/man.vim
runtime macros/matchit.vim
" VUNDLE rtp, @see https://github.com/gmarik/vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-commentary'
" git syntax, indent, and filetypes
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-surround'
Bundle 'Tagbar'
Bundle 'jiangmiao/auto-pairs'
Bundle 'Valloric/YouCompleteMe'
Bundle 'Superbil/llvm.vim'
" Bundle 'Cpp11-Syntax-Support'
" Bundle 'chrisbra/csv.vim'
" Bundle 'ack.vim'
"
" colorscheme valloric

" ycm
let g:ycm_confirm_extra_conf = 0
let g:ycm_global_ycm_extra_conf = '~/.bin/.ycm_extra_conf.py'
let g:ycm_complete_in_strings = 1
" clang_complete
" let g:clang_auto_select=0
" let g:clang_complete_auto=1
" let g:clang_complete_copen=1
" let g:clang_hl_errors=1
" let g:clang_periodic_quickfix=0
" let g:clang_snippets=1
" let g:clang_snippets_engine="clang_complete"
" let g:clang_conceal_snippets=1
" let g:clang_exec="clang"
" let g:clang_user_options=""
" let g:clang_auto_user_options=".clang_complete,path"
" let g:clang_use_library=1
" let g:clang_library_path="/usr/lib/llvm-3.4/lib/"
" let g:clang_sort_algo="priority"
" let g:clang_complete_macros=1
" let g:clang_complete_patterns=0
" nnoremap <Leader>q :call g:ClangUpdateQuickFix()<CR>

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

" gf issues; set suffixesadd+=
set includeexpr=GuessFilename(v:fname)
"if gf cannot find the path, strip starting '.' or '..'
function! GuessFilename(filename)
   let ret = ''
   if a:filename =~ '^\.\/\.\.\/'
     let ret = strpart(a:filename, 5)
   elseif a:filename =~ '^\.\.\/'
     let ret = strpart(a:filename, 3)
   elseif a:filename =~ '^\.\/'
     let ret = strpart(a:filename, 2)
   endif
   if len(ret)
     return ret
   else
     return a:filename
   endif
endfunction 

"append modeline
function! AppendModeline()
  let l:modeline = printf(" vim: set ft=%s ts=%d sw=%d tw=%d %set :",
        \ &filetype, &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("0"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

" http://clang.llvm.org/docs/HowToSetupToolingForLLVM.html#using-clang-tools
function! ClangCheckImpl(cmd)
  if &autowrite | wall | endif
  echo "Running " . a:cmd . " ..."
  let l:output = system(a:cmd)
  cexpr l:output
  cwindow
  let w:quickfix_title = a:cmd
  if v:shell_error != 0
    cc
  endif
  let g:clang_check_last_cmd = a:cmd
endfunction

" cannot work since an additional -- is needed
function! ClangCheck()
  let l:filename = expand('%')
  if l:filename =~ '\.\(cpp\|cxx\|cc\|c\)$'
    call ClangCheckImpl("clang-check " . l:filename)
  elseif exists("g:clang_check_last_cmd")
    call ClangCheckImpl(g:clang_check_last_cmd)
  else
    echo "Can't detect file's compilation arguments and no previous clang-check invocation!"
  endif
endfunction

nmap <silent> <F5> :call ClangCheck()<CR>

" =====================================================
" filetype settings
" =====================================================
autocmd FileType c,cpp setl path+=/usr/lib/llvm-2.9/include,/usr/include/c++/4.6,~/moonbox/klee/include
autocmd FileType c,cpp setl cms=//%s
autocmd FileType c,cpp nnoremap <silent> <C-]> :YcmCompleter GoToDefinitionElseDeclaration<CR>
autocmd FileType lisp setl cms=;;%s
autocmd FileType cmake setl cms=#%s
