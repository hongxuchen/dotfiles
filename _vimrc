" vim: set ft=vim ts=4 sw=2 tw=78 et :

source ~/.vread

" save and reload ~/.vimrc
nnoremap <silent> <leader>v :w<CR>:source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>
" sudo write this
cnoremap W! w !sudo tee % >/dev/null
" Paste from clipboard
set clipboard=unnamed "Use system clipboard ("*)
nnoremap <leader>p :set paste! <CR>
" Remove trailing whitespace
nnoremap <leader>f :%s/\s\+$//<cr>:let @/=''<CR>
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

autocmd FileType c,cpp,java,markdown autocmd BufWritePre <buffer> :%s/\s\+$//e
nnoremap gO :!open <cfile><CR>

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

set ttyfast
set colorcolumn=120
set autochdir
set formatoptions=tq
set noautowrite               " Never write a file unless I request it.
set noautowriteall            " NEVER.
set autoread                  " automatically re-read changed files.
set confirm                   " Y-N-C prompt if closing with unsaved changes.
set laststatus=2              " Always show statusline, even if only 1 window.
" set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%=%-16(\ %l,%c-%v\ %)%P
" set statusline+=%#warningmsg#%{SyntasticStatuslineFlag()}%*

" runtime ftplugin/man.vim
runtime macros/matchit.vim
" additional plugins
Bundle 'indentpython.vim--nianyang'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rhubarb'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-tbone'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-jdaddy'
Bundle 'tpope/vim-dispatch'
Bundle 'jiangmiao/auto-pairs'

Bundle 'bling/vim-airline'
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#left_sep = ' '
" let g:airline#extensions#tabline#left_alt_sep = '|'

Bundle 'tomasr/molokai'
colorscheme molokai

Bundle 'tpope/vim-commentary'
autocmd FileType lisp setl cms=;;%s
autocmd FileType cmake setl cms=#%s
autocmd FileType xdefaults setl cms=!%s
autocmd FileType gdb setl cms=#%s
autocmd FileType c setl cms=//%s
autocmd FileType cpp setl cms=//%s
autocmd FileType tablegen setl cms=//%s
autocmd FileType unix setl cms=#%s

" tagbar
Bundle 'Tagbar'
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
Bundle 'scrooloose/nerdtree'
noremap <leader>T :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']
let g:NERDTreeDirArrows=0

" append modeline
function! AppendModeline()
  let l:modeline = printf(" vim: set ft=%s ts=%d sw=%d tw=%d %set :",
        \ &filetype, &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("0"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

" Bundle 'kien/ctrlp.vim'
" inoremap <silent><C-x>b <C-o>:CtrlPMRUFiles<CR>
" let g:ctrlp_map = '<c-p>'
" let g:ctrlp_cmd = 'CtrlP'
" let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
" let g:ctrlp_working_path_mode = 'ra'

Bundle 'scrooloose/syntastic'
" let g:syntastic_auto_loc_list = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_zsh_checkers = ['zsh']
let g:syntastic_mode_map = {
    \ "mode": "active",
    \ "active_filetypes": ["ruby", "python"],
    \ "passive_filetypes": ["c",'cpp', 'java', 'cs'] }

" YouCompleteMe
Bundle 'Valloric/YouCompleteMe'
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
let g:ycm_filetype_blacklist = {
      \ 'tagbar' : 1,
      \ 'qf' : 1,
      \ 'notes' : 1,
      \ 'markdown' : 1,
      \ 'unite' : 1,
      \ 'text' : 1,
      \ 'vimwiki' : 1,
      \ 'gitcommit' : 1,
      \}

" javascript tern
Bundle 'marijnh/tern_for_vim'
" let tern#is_show_argument_hints_enabled = 1
" autocmd FileType javascript setlocal omnifunc=tern#Complete
