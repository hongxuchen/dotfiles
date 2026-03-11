local fzf = require("fzf-lua")

fzf.setup {
  { "telescope", "fzf-native" },
  winopts = {
    height = 0.95,
    width = 0.95,
    row = 0.5,
    col = 0.5,
    border = "single",
    preview = { default = "bat_native", border = "rounded" },
    treesitter = false,
  },
  fzf_opts = { ["--ansi"] = true, ["--cycle"] = true },
  defaults = {
    file_icons = false,
  },
  previewers = {
    builtin = {
      syntax = true,
      syntax_limit_b = 1024 * 100, -- 100KB
      extensions = {
        ["png"] = { "viu", "-b", "{file}" },
        ["jpg"] = { "viu", "-b", "{file}" },
        ["jpeg"] = { "viu", "-b", "{file}" },
      },
    },
  },
  grep = {
    actions = {
      ["ctrl-g"] = { require("fzf-lua.actions").toggle_ignore },
    },
  },
  oldfiles = {
    include_current_session = true,
  },
  files = {
    git_icons = true,
    cwd_prompt_shorten_len = 24,
    -- formatter = "path.filename_first",
  },
  manpages = {
    previewer = "man_native",
  },
  helptags = {
    previewer = "help_native",
  },
}

fzf.register_ui_select()
