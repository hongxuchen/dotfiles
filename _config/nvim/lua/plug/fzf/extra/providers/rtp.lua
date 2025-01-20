return function(opts)
  local default = {
    previewer = {
      cmd = "eza --color=always --tree --level=3 --icons=always -- {}",
      _ctor = require("fzf-lua.previewer").fzf.cmd,
    },
    path_shorten = "set-to-trigger-glob-expansion",
    actions = {
      ["enter"] = function(sel)
        require("plug.fzf.extra.utils").zoxide_chdir(sel[1])
      end,
      ["ctrl-l"] = function(sel)
        require("fzf-lua").files { cwd = sel[1] }
      end,
      ["ctrl-n"] = function(sel)
        require("fzf-lua").live_grep_native { cwd = sel[1] }
      end,
    },
  }
  opts = vim.tbl_extend("force", default, opts or {})
  local clear = require("fzf-lua").utils.ansi_escseq.clear
  local clear_pat = vim.pesc(clear)
  local contents = vim
    .iter(vim.api.nvim_list_runtime_paths())
    :map(require("plug.fzf.extra.utils").replace_with_envname)
    :map(function(path) -- hack...
      local cleared = path:gsub(clear_pat, "")
      return cleared and cleared .. clear or path
    end)
    :totable()

  return require("fzf-lua").fzf_exec(contents, opts)
end
