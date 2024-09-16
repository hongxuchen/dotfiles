## Trouble shooting Language Servers
* `LspInfo` and see the following settings
    * whether the corresponding LS has been running
    * whether the root directory is correctly found
    * whether the file type matches
* `LspLog`
    * whether there are error messages about LS

## Trouble shooting with treesitter
* upgrade `nvim-treesitter` parsers by removing `parser` directory inside
* remove `plug/treesitter` directory

## requirements
* [ripgrep](https://github.com/BurntSushi/ripgrep)
* [zoxide](https://github.com/ajeetdsouza/zoxide)

## Issues
* statusline works correctly only when `laststatus=3`
* lua lsp may return multi-language docs
* null-ls only filetypes have no lsp (diagnostic/format) keymaps
* `my_lsp_rename` not work for python
* `workspace_folders` not working correctly?
* floating window height may be zero, which causes error
* mason treats existence of `packages` of available, which is incorrect
* `lsp.util.jump_to_location` may jump to a random position first when the target file is not opened (relevant with "lcd %:p:h" or telescope, or caused by other plugins, or due to `reuse_win==false`?)
* lazy.nvim works wrongly with lsp/init.lua (require issues) when there are plugin changes

## TODOs
* write LSIF format parser and navigator
* set `iskeyword` correctly when inside treesitter injection
* semantic sort (on paragraph/function)

## Learn
* vim basics: https://learnvim.irian.to/
* luvref: `:help luvref`
* [Lua stdlib feature parity against Vimscript](https://github.com/neovim/neovim/issues/18393)
* [vimregex](https://vimregex.com/)

