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

local u = require("core.utils")

u.keymap({ "n", "v", "i" }, "<C-x><C-f>", function()
  require("fzf-lua").complete_path()
end, u.opts, "[fzf] Fuzzy complete path")

u.keymap("n", "<leader>f", function()
  fzf.files()
end, u.opts, "[fzf] find files in cur dir")

u.keymap("n", "<leader>ww", function()
  fzf.files {
    cwd = u.get_workspace_root(),
  }
end, u.opts, "[fzf] find files in workspace")

u.keymap("n", "<leader>bl", fzf.blines, u.opts, "[fzf] fuzzy grep in cur buffer")

u.keymap("n", "<leader>wl", function()
  fzf.live_grep_glob { cwd = vim.uv.cwd() }
end, u.opts, "[fzf] live grep in workspace")
u.keymap("n", "<leader>ws", function()
  fzf.grep_cword { cwd = u.get_workspace_root() }
end, u.opts, "[fzf] grep cur word in workspace")
u.keymap("n", "<leader>wS", function()
  fzf.grep_cword { cwd = u.get_workspace_root() }
end, u.opts, "[fzf] grep cur WORD in workspace")
u.keymap("v", "<leader>ws", function()
  fzf.grep_visual { cwd = u.get_workspace_root() }
end, u.opts, "[fzf] visual grep cur word in workspace")

u.keymap("n", "<leader>dl", function()
  fzf.live_grep_glob { cwd = vim.uv.cwd() }
end, u.opts, "[fzf] live grep in cur dir")
u.keymap("n", "<leader>ds", function()
  fzf.grep_cword { cwd = vim.uv.cwd() }
end, u.opts, "[fzf] grep cur word in cur dir")
u.keymap("n", "<leader>dS", function()
  fzf.grep_cword { cwd = vim.uv.cwd() }
end, u.opts, "[fzf] grep cur WORD in cur dir")
u.keymap("v", "<leader>ds", function()
  fzf.grep_visual { cwd = vim.uv.cwd() }
end, u.opts, "[fzf] visual grep cur word in cur dir")

u.keymap("n", "<leader>m", function()
  fzf.manpages {}
end, u.opts, "[fzf] search manpages")

u.keymap("n", "<leader>z", fzf.zoxide, u.opts, "[fzf] go to recent folders")

u.keymap("n", "<leader>r", fzf.resume, u.opts, "[fzf] resume last picker")

u.keymap("n", "<leader>gs", fzf.git_status, u.opts, "[fzf] git status")
u.keymap("n", "<leader>h", fzf.helptags, u.opts, "[fzf] search help tags")
u.keymap("n", "<leader>o", fzf.oldfiles, u.opts, "[fzf] find old files")
u.keymap("n", "<leader>bb", function()
  fzf.buffers {}
end, u.opts, "[fzf] search buffers")

u.keymap("n", '<leader>"', fzf.registers, u.opts, "[fzf] show registers content")

u.keymap("n", "<leader>tl", fzf.tabs, u.opts, "[fzf] open buffers in tabs")

u.keymap("n", "<leader><leader>", function()
  fzf.builtin {}
end, u.opts, "[fzf] run builtin pickers")

u.keymap("n", "gs", fzf.lsp_document_symbols, u.opts, "[fzf] lsp document symbols")

u.keymap("n", "<leader>ddc", fzf.dap_commands, u.opts, "[fzf] find dap_commands")
u.keymap("n", "<leader>dds", fzf.dap_commands, u.opts, "[fzf] find dap_configurations")
u.keymap("n", "<leader>ddb", fzf.dap_commands, u.opts, "[fzf] find dap_breakpoints")
u.keymap("n", "<leader>ddv", fzf.dap_commands, u.opts, "[fzf] find dap_variables")
u.keymap("n", "<leader>ddf", fzf.dap_commands, u.opts, "[fzf] find dap_frames")
