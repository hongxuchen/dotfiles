-- https://github.com/JoosepAlviste/dotfiles/blob/c171efbbbe0daa5e737250ec82338a51ed53c15a/config/nvim/ftplugin/json.lua
vim.keymap.set("n", "o", function()
  local line = vim.api.nvim_get_current_line()

  local should_add_comma = string.find(line, "[^,{[]$")
  if should_add_comma then
    return "A,<cr>"
  else
    return "o"
  end
end, { buffer = true, expr = true })
