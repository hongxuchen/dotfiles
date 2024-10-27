-- make it work for 'sudo'
if vim.env.SUDO_USER then
  --- @type string
  local extra_rtp = "/home/" .. vim.env.SUDO_USER .. "/.config/nvim"
  vim.opt.runtimepath:append { extra_rtp }
end

require("core.options")

vim.cmd.colorscheme("desert")
