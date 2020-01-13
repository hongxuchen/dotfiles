"vim: set ft=vim ts=4 sw=2 tw=78 et :

" PRINCIPLES:
" keep vim as simple as possible
" plugins should be loaded as lazy as possible
" startup should be as fast as possible

""" read-only settings
source ~/.vread

""" MISC settings
" save and reload ~/.vimrc
nnoremap <silent> <leader>v :w<CR>:source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>
vnoremap <leader>vs y:@"<CR>
" C-j to insert a newline
nnoremap <NL> i<CR><ESC>
" refresh if file in Vim is updated by external program,TODO
noremap <silent><F5> :checktime<CR>:exe ":echo 'file refreshed'"<CR>
inoremap <silent><F5> <C-O>:checktime<CR>:exe ":echo 'file refreshed'"<CR>

""" Insert-mode keybindings
inoremap <silent><C-x>0 <C-o>:hide<CR>
inoremap <silent><C-x>1 <C-o>:hide :only<CR>
inoremap <silent><C-e> <C-o>$
inoremap <silent><C-a> <C-o>0
inoremap <silent><C-f> <Right>
inoremap <silent><C-b> <Left>
inoremap <silent><C-d> <Del>
inoremap <silent><M-n> <C-o>:cnext<CR>
inoremap <silent><M-p> <C-o>:cprevious<CR>

""" Ex-mode navigation
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <silent><C-g> <ESC><ESC>

""" terminal mode
if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>
  highlight! link TermCursor Cursor
  highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15
end

""" append modeline
function! AppendModeline()
  let l:modeline = printf(" vim: set ft=%s ts=%d sw=%d tw=%d %set :",
        \ &filetype, &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("0"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

set ttyfast
set clipboard^=unnamed,unnamedplus
set colorcolumn=
set noautowrite               " Never write a file unless I request it.
set noautowriteall            " NEVER.
set autoread                  " automatically re-read changed files.
set confirm                   " Y-N-C prompt if closing with unsaved changes.
set laststatus=2              " Always show statusline, even if only 1 window.
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%=%-16(\ %l,%c-%v\ %)%P
set complete-=t
set formatoptions=jql
set spellfile=~/.vim/spell/en.utf-8.add
set background=light

autocmd FileType c,cpp,java,markdown autocmd BufWritePre <buffer> :%s/\s\+$//e
autocmd FileType html,eruby,rb,css,js,xml runtime! macros/matchit.vim
colorscheme desert


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/bundle')

" Plug 'Superbil/llvm.vim', {'for': 'llvm'}
" Plug 'vim-scripts/scons.vim', {'for': 'scons'}
" Plug 'tomlion/vim-solidity', {'for': 'solidity'}
" Plug 'cespare/vim-toml', {'for': 'toml'}
" Plug 'plasticboy/vim-markdown'

"vim-g
Plug 'szw/vim-g'
let g:vim_g_f_command = "Gf"
let g:vim_g_command = "Go"

Plug 'editorconfig/editorconfig-vim'
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*', 'term://.*']

" enhanced netrw
Plug 'tpope/vim-vinegar'
" enhance Vim UNIX experience
Plug 'tpope/vim-eunuch'
" git relevant
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
" enhance on surroundings
Plug 'tpope/vim-surround'
" pairs
Plug 'jiangmiao/auto-pairs'
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<M-b>'

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
autocmd FileType apache setlocal commentstring=#\ %s

Plug 'jremmen/vim-ripgrep'

""" tagbar display
Plug 'majutsushi/tagbar'
noremap <leader>t :TagbarToggle<CR>
let g:tagbar_autoclose=0
let g:tagbar_left = 1
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


""" startify
Plug 'mhinz/vim-startify'
let g:startify_custom_header = []
let g:startify_change_to_vcs_root = 1
let g:startify_enable_unsafe = 1
let g:startify_update_oldfiles = 1
let g:startify_files_number = 20
let g:startify_enable_special = 0

Plug 'w0rp/ale'
let g:ale_sign_column_always = 1
let g:ale_sign_error = 'x'
let g:ale_sign_warning = '!'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_filetype_changed = 0
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_keep_list_window_open = 0
" let g:ale_open_list = 1
let g:ale_linters = {
  \   'csh': ['shell'],
  \   'elixir': ['credo', 'dialyxir', 'dogma'],
  \   'go': ['gofmt', 'golint', 'go vet'],
  \   'hack': ['hack'],
  \   'help': [],
  \   'perl': ['perlcritic'],
  \   'perl6': [],
  \   'python': [],
  \   'rust': ['cargo'],
  \   'spec': [],
  \   'text': [],
  \   'vue': ['eslint', 'vls'],
  \   'zsh': ['shell'],
  \}
Plug 'tell-k/vim-autopep8', {'for': 'python'}

Plug 'neoclide/coc.nvim', {'branch': 'release'}

""" latex
Plug 'lervag/vimtex'
Plug 'mhinz/neovim-remote'
let g:vimtex_compiler_progname='nvr'
let g:vimtex_quickfix_open_on_warning = 0
let g:vimtex_view_general_viewer = 'zathura'
set conceallevel=1
let g:tex_conceal='abdmg'
let g:vimtex_compiler_callback_hooks = ['MyTestHook']
function! MyTestHook(status)
    echom a:status
endfunction

Plug 'AndrewRadev/splitjoin.vim'
Plug 'junegunn/vim-easy-align'
" Chinese typesetting
Plug 'hotoo/pangu.vim'

call plug#end()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""" configurations for coc
" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup
" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=400
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
