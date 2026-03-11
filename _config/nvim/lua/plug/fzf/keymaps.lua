local u = require("core.utils")

---@param method string
---@param opts table<string, any>|fun(): table<string, any>|nil
---@return fun()
local function with_fzf(method, opts)
  return function()
    local fzf = require("fzf-lua")
    local resolved = type(opts) == "function" and opts() or opts
    if resolved == nil then
      return fzf[method]()
    end
    return fzf[method](resolved)
  end
end

u.keymap({ "n", "v", "i" }, "<C-x><C-f>", with_fzf("complete_path"), u.opts, "[fzf] Fuzzy complete path")

u.keymap("n", "<leader>f", with_fzf("files"), u.opts, "[fzf] find files in cur dir")
u.keymap(
  "n",
  "<leader>ww",
  with_fzf("files", function()
    return {
      cwd = u.get_workspace_root(),
    }
  end),
  u.opts,
  "[fzf] find files in workspace"
)

u.keymap("n", "<leader>bl", with_fzf("blines"), u.opts, "[fzf] fuzzy grep in cur buffer")

u.keymap(
  "n",
  "<leader>wl",
  with_fzf("live_grep_glob", function()
    return { cwd = vim.uv.cwd() }
  end),
  u.opts,
  "[fzf] live grep in workspace"
)
u.keymap(
  "n",
  "<leader>ws",
  with_fzf("grep_cword", function()
    return { cwd = u.get_workspace_root() }
  end),
  u.opts,
  "[fzf] grep cur word in workspace"
)
u.keymap(
  "n",
  "<leader>wS",
  with_fzf("grep_cword", function()
    return { cwd = u.get_workspace_root() }
  end),
  u.opts,
  "[fzf] grep cur WORD in workspace"
)
u.keymap(
  "v",
  "<leader>ws",
  with_fzf("grep_visual", function()
    return { cwd = u.get_workspace_root() }
  end),
  u.opts,
  "[fzf] visual grep cur word in workspace"
)

u.keymap(
  "n",
  "<leader>dl",
  with_fzf("live_grep_glob", function()
    return { cwd = vim.uv.cwd() }
  end),
  u.opts,
  "[fzf] live grep in cur dir"
)
u.keymap(
  "n",
  "<leader>ds",
  with_fzf("grep_cword", function()
    return { cwd = vim.uv.cwd() }
  end),
  u.opts,
  "[fzf] grep cur word in cur dir"
)
u.keymap(
  "n",
  "<leader>dS",
  with_fzf("grep_cword", function()
    return { cwd = vim.uv.cwd() }
  end),
  u.opts,
  "[fzf] grep cur WORD in cur dir"
)
u.keymap(
  "v",
  "<leader>ds",
  with_fzf("grep_visual", function()
    return { cwd = vim.uv.cwd() }
  end),
  u.opts,
  "[fzf] visual grep cur word in cur dir"
)

u.keymap("n", "<leader>m", with_fzf("manpages"), u.opts, "[fzf] search manpages")
u.keymap("n", "<leader>z", with_fzf("zoxide"), u.opts, "[fzf] go to recent folders")
u.keymap("n", "<leader>r", with_fzf("resume"), u.opts, "[fzf] resume last picker")
u.keymap("n", "<leader>gs", with_fzf("git_status"), u.opts, "[fzf] git status")
u.keymap("n", "<leader>h", with_fzf("helptags"), u.opts, "[fzf] search help tags")
u.keymap("n", "<leader>o", with_fzf("oldfiles"), u.opts, "[fzf] find old files")
u.keymap("n", "<leader>bb", with_fzf("buffers"), u.opts, "[fzf] search buffers")
u.keymap("n", '<leader>"', with_fzf("registers"), u.opts, "[fzf] show registers content")
u.keymap("n", "<leader>tl", with_fzf("tabs"), u.opts, "[fzf] open buffers in tabs")
u.keymap("n", "<leader><leader>", with_fzf("builtin"), u.opts, "[fzf] run builtin pickers")
u.keymap("n", "gs", with_fzf("lsp_document_symbols"), u.opts, "[fzf] lsp document symbols")

u.keymap("n", "<leader>ddc", with_fzf("dap_commands"), u.opts, "[fzf] find dap_commands")
u.keymap("n", "<leader>dds", with_fzf("dap_configurations"), u.opts, "[fzf] find dap_configurations")
u.keymap("n", "<leader>ddb", with_fzf("dap_breakpoints"), u.opts, "[fzf] find dap_breakpoints")
u.keymap("n", "<leader>ddv", with_fzf("dap_variables"), u.opts, "[fzf] find dap_variables")
u.keymap("n", "<leader>ddf", with_fzf("dap_frames"), u.opts, "[fzf] find dap_frames")

u.keymap("n", "<leader>ll", "<Cmd>LazyPlugins<CR>", u.opts, "[fzf] list lazy plugins")
u.keymap("n", "<leader>li", "<Cmd>FzfLspInfo<CR>", u.opts, "[fzf] LSP info")
