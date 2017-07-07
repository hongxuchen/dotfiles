nnoremap <leader>f :call Autopep8()<CR>
let g:autopep8_disable_show_diff=1
autocmd FileType python set equalprg=autopep8\ -
