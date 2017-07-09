nnoremap <silent> <buffer> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> <buffer> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <buffer> <F2> :call LanguageClient_textDocument_rename()<CR>
" nnoremap <silent> gd :YcmCompleter GoTo<CR>
" nnoremap <silent> K :YcmCompleter GetDoc<CR>
setlocal formatprg=rustfmt
setlocal formatoptions=tq
