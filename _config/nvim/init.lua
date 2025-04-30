local plugin_dir = "plug"

-- for plugin debugging purpose, setting DEBUG_NVIM env
if vim.env.DEBUG_NVIM then
  plugin_dir = "debug"
else
  require("core.options")
  require("core.keymaps")
  require("core.autocmds")
  require("core.mycmds")
  vim.o.statusline = [[%!luaeval('require("status").statusline()')]]
end

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  local clone_cmd = {
    "git",
    "clone",
    "--filter=blob:none",
    "--single-branch",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  }
  local clone_run = vim.system(clone_cmd, { text = true })
  local res = clone_run:wait()
  if res.code ~= 0 or res.signal ~= 0 then
    vim.notify(string.format("error during git clone: %s", res.stderr), vim.log.levels.ERROR, {})
    do
      return
    end
  end
  local checkout_run = vim.system({ "git", "-C", lazypath, "checkout", "tags/stable" }, { text = true })
  res = checkout_run:wait()
  if res.code ~= 0 or res.signal ~= 0 then
    vim.notify(string.format("error during git check: %s", res.stderr), vim.log.levels.ERROR, {})
    do
      return
    end
  end
end

vim.opt.runtimepath:prepend(lazypath)
require("lazy").setup {
  spec = {
    { import = plugin_dir },
  },
  dev = {
    path = "~/tools",
    pattern = { "clangd_extensions" },
    fallback = true,
  },
  defaults = {
    lazy = false,
    version = nil,
  },
  rtp = {
    reset = true,
    disabled_plugins = {},
  },
  readme = {
    enabled = false,
    skip_if_doc_exists = true,
  },
  checker = {
    enabled = false,
    frequency = 86400,
  },
  git = {
    log = { "--since=2 days ago" },
    timeout = 60,
  },
  change_detection = {
    enabled = true,
    notify = false,
  },
}

-- vim.cmd.colorscheme("wildcharm")
-- vim.cmd.colorscheme("retrobox")
vim.cmd.colorscheme("onedark")
