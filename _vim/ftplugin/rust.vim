" nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
" nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
" nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>
" autocmd FileType rust set equalprg=rustfmt
nnoremap <silent> gd :YcmCompleter GoTo<CR>
nnoremap <silent> K :YcmCompleter GetDoc<CR>
setlocal formatoptions=
