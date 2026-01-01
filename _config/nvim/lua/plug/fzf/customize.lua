local fzf = require("fzf-lua")
local ations = fzf.actions

local mycmd = vim.api.nvim_create_user_command

--- ENV picker
local bpreviewer = require("fzf-lua.previewer.builtin")
local EnvPreviewer = bpreviewer.base:extend()

function EnvPreviewer:new(o, opts, fzf_win)
  EnvPreviewer.super.new(self, o, opts, fzf_win)
  setmetatable(self, EnvPreviewer)
  return self
end

function EnvPreviewer:populate_preview_buf(entry_str)
  local tmpbuf = self:get_tmp_buffer()
  local entry = vim.system({ "printenv", entry_str }, { text = true }):wait().stdout:gsub("[\n\r]", " ")
  vim.api.nvim_buf_set_lines(tmpbuf, 0, -1, false, {
    " " .. entry,
  })
  self:set_preview_buf(tmpbuf)
  -- self.win:update_scrollbar()
end

function EnvPreviewer:gen_winopts()
  local new_winopts = {
    wrap = true,
    number = false,
  }
  return vim.tbl_extend("force", self.winopts, new_winopts)
end

local function printenv()
  local cmd = "printenv | cut -d= -f1"
  local opts = {
    prompt = ":",
    previewer = EnvPreviewer,
    hls = { cursorline = "" },
    winopts = {
      title = " env variables ",
      title_pos = "center",
      height = 0.4,
      preview = {
        hidden = "nohidden",
        horizontal = "down:5%",
      },
    },
    actions = {
      ["default"] = function(selected)
        vim.notify(
          vim.system({ "printenv", selected[1] }, { text = true }):wait().stdout:gsub("[\n\r]", " "),
          vim.log.levels.INFO,
          { ft = "bash" }
        )
      end,
    },
  }
  fzf.fzf_exec(cmd, opts)
end

mycmd("Env", printenv, {})

-- zoxide picker TODO:

local list_zoxide = function(action, selected, o) end

-- git-file picker

local list_files_from_branch_action = function(action, selected, o)
  local file = fzf.path.entry_to_file(selected[1], o)
  local cmd = string.format("%s %s:%s", action, o.args, file.path)
  vim.cmd(cmd)
end

mycmd("ListFilesFromBranch", function(opts)
  fzf.files {
    cmd = "git ls-tree -r --name-only " .. opts.args,
    prompt = opts.args .. "> ",
    actions = {
      ["default"] = function(selected, o)
        list_files_from_branch_action("Gedit", selected, o)
      end,
      ["ctrl-s"] = function(selected, o)
        list_files_from_branch_action("Gsplit", selected, o)
      end,
      ["ctrl-v"] = function(selected, o)
        list_files_from_branch_action("Gvsplit", selected, o)
      end,
    },
    previewer = false,
    preview = {
      type = "cmd",
      fn = function(items)
        local file = fzf.path.entry_to_file(items[1])
        return string.format("git diff %s HEAD -- %s | delta", opts.args, file.path)
      end,
    },
  }
end, {
  nargs = 1,
  force = true,
  complete = function()
    local res = vim.system({ "git", "branch", "--all", "--sort=-committerdate" }, { text = true }):wait()
    if res.code == 0 then
      local branches = vim.split(res.stdout, "\n", { plain = true, trimempty = true })
      return vim.tbl_map(function(x)
        return x:match("[^%s%*]+"):gsub("^remotes/", "")
      end, branches)
    else
      return {}
    end
  end,
})

-- aerial picker
local function pick_document_symbols()
  local status_ok, aerial_fzf = pcall(require, "aerial.fzf")
  if not status_ok then
    vim.notify("aerial not installed, fallback to lsp_document_symbols", vim.log.levels.WARN)
    fzf.lsp_document_symbols()
    return
  end
  local aerial_labels = aerial_fzf.get_labels()
  require("aerial").setup {}
  local aerial_navigation = require("aerial.navigation")
  local aerial_data = require("aerial.data")
  local function goto_symbol(symbol)
    local idx = tonumber(symbol:match("^(%d+)"))
    for i, _, symbol_idx in aerial_data.get_or_create(0):iter { skip_hidden = true } do
      if idx == i then
        aerial_navigation.select {
          index = symbol_idx,
        }
        break
      end
    end
  end
  local opts = {
    prompt = ":",
    previewer = bpreviewer,
    hls = { cursorline = "" },
    winopts = {
      title = " Document Symbols ",
      title_pos = "center",
      height = 0.6,
      width = 0.4,
    },
    actions = {
      ["default"] = function(selected)
        goto_symbol(selected[1])
      end,
    },
  }
  fzf.fzf_exec(aerial_labels, opts)
end

mycmd("DocumentSymbols", pick_document_symbols, {})

-- lazy plugins picker
mycmd("LazyPlugins", function()
  local lazy_config = require("lazy.core.config")
  local plugins = {}

  for name, plugin in pairs(lazy_config.plugins) do
    local loaded = plugin._.loaded
    table.insert(plugins, {
      name = name,
      loaded = loaded,
      dir = plugin.dir and vim.fn.fnamemodify(plugin.dir, ":~") or "",
    })
  end

  table.sort(plugins, function(a, b)
    return a.name < b.name
  end)

  -- Build entries with the plugin name at the start for easier extraction
  local entries = {}
  for _, p in ipairs(plugins) do
    local status = p.loaded and "✓" or "✗"
    table.insert(entries, string.format("%s\t%s\t%s", p.name, status, p.dir))
  end

  fzf.fzf_exec(entries, {
    prompt = "Lazy Plugins> ",
    previewer = false,
    winopts = {
      title = " Lazy Plugins ",
      title_pos = "center",
      height = 0.6,
      width = 0.8,
    },
    actions = {
      ["default"] = function(selected, _)
        -- Extract plugin name and dir (third field after second tab)
        local parts = vim.split(selected[1], "\t")
        local name = parts[1]
        local dir = vim.fn.expand(parts[3])

        -- Change to plugin directory and open it in fzf files
        if dir and vim.fn.isdirectory(dir) ~= 0 then
          vim.cmd("cd " .. vim.fn.fnameescape(dir))
          fzf.files()
        end
      end,
    },
  })
end, {})

-- LSP Info picker
mycmd("LspInfo", function()
  local bufnr = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients({ bufnr = bufnr })

  if #clients == 0 then
    vim.notify("No LSP clients attached to this buffer", vim.log.levels.WARN)
    return
  end

  local entries = {}
  for _, client in ipairs(clients) do
    local name = client.name
    local id = client.id
    local root_dir = client.config.root_dir and vim.fn.fnamemodify(client.config.root_dir, ":~") or "N/A"

    -- Get client capabilities summary
    local caps = client.server_capabilities
    local features = {}
    if caps.completionProvider then table.insert(features, "completion") end
    if caps.definitionProvider then table.insert(features, "definition") end
    if caps.typeDefinitionProvider then table.insert(features, "typeDef") end
    if caps.referencesProvider then table.insert(features, "references") end
    if caps.implementationProvider then table.insert(features, "implementation") end
    if caps.renameProvider then table.insert(features, "rename") end
    if caps.documentFormattingProvider then table.insert(features, "format") end
    if caps.documentRangeFormattingProvider then table.insert(features, "rangeFormat") end
    if caps.documentSymbolProvider then table.insert(features, "symbols") end
    if caps.workspaceSymbolProvider then table.insert(features, "workspace") end
    if caps.codeActionProvider then table.insert(features, "codeAction") end
    if caps.codeLensProvider then table.insert(features, "codeLens") end
    if caps.inlayHintProvider then table.insert(features, "inlayHints") end

    local feature_str = #features > 0 and table.concat(features, ", ") or "basic"

    table.insert(entries, string.format("%s\t[%d]\t%s\t%s", name, id, root_dir, feature_str))
  end

  -- Custom previewer for LSP details
  local LspPreviewer = bpreviewer.base:extend()

  function LspPreviewer:new(o, opts, fzf_win)
    LspPreviewer.super.new(self, o, opts, fzf_win)
    setmetatable(self, LspPreviewer)
    return self
  end

  function LspPreviewer:populate_preview_buf(entry_str)
    local tmpbuf = self:get_tmp_buffer()
    local parts = vim.split(entry_str, "\t")
    local client_name = parts[1]

    -- Find the client
    local client = nil
    for _, c in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
      if c.name == client_name then
        client = c
        break
      end
    end

    if not client then
      vim.api.nvim_buf_set_lines(tmpbuf, 0, -1, false, { "Client not found" })
      self:set_preview_buf(tmpbuf)
      return
    end

    local lines = {}
    table.insert(lines, " " .. client.name .. " (id: " .. client.id .. ")")
    table.insert(lines, "")
    table.insert(lines, " Root Directory:")
    table.insert(lines, "   " .. (client.config.root_dir or "N/A"))
    table.insert(lines, "")
    table.insert(lines, " Command:")
    table.insert(lines, "   " .. vim.inspect(client.config.cmd):gsub("\n", " "))
    table.insert(lines, "")
    table.insert(lines, " Filetypes:")
    if client.config.filetypes then
      for _, ft in ipairs(client.config.filetypes) do
        table.insert(lines, "   - " .. ft)
      end
    else
      table.insert(lines, "   (auto-detected)")
    end
    table.insert(lines, "")
    table.insert(lines, " Root Markers:")
    if client.config.root_markers then
      for _, marker in ipairs(client.config.root_markers) do
        table.insert(lines, "   - " .. marker)
      end
    else
      table.insert(lines, "   (none)")
    end
    table.insert(lines, "")
    table.insert(lines, " Server Capabilities:")
    local caps = client.server_capabilities
    if caps.completionProvider then table.insert(lines, "   ✓ Completion") end
    if caps.definitionProvider then table.insert(lines, "   ✓ Definition") end
    if caps.typeDefinitionProvider then table.insert(lines, "   ✓ Type Definition") end
    if caps.referencesProvider then table.insert(lines, "   ✓ References") end
    if caps.implementationProvider then table.insert(lines, "   ✓ Implementation") end
    if caps.renameProvider then table.insert(lines, "   ✓ Rename") end
    if caps.documentFormattingProvider then table.insert(lines, "   ✓ Document Formatting") end
    if caps.documentRangeFormattingProvider then table.insert(lines, "   ✓ Range Formatting") end
    if caps.documentSymbolProvider then table.insert(lines, "   ✓ Document Symbols") end
    if caps.workspaceSymbolProvider then table.insert(lines, "   ✓ Workspace Symbols") end
    if caps.codeActionProvider then table.insert(lines, "   ✓ Code Actions") end
    if caps.codeLensProvider then table.insert(lines, "   ✓ Code Lens") end
    if caps.inlayHintProvider then table.insert(lines, "   ✓ Inlay Hints") end
    if caps.documentHighlightProvider then table.insert(lines, "   ✓ Document Highlight") end

    vim.api.nvim_buf_set_lines(tmpbuf, 0, -1, false, lines)
    self:set_preview_buf(tmpbuf)
  end

  function LspPreviewer:gen_winopts()
    return {
      wrap = false,
      number = false,
      relativenumber = false,
    }
  end

  fzf.fzf_exec(entries, {
    prompt = "LSP> ",
    previewer = LspPreviewer,
    fzf_opts = { ["--no-hscroll"] = true },
    winopts = {
      title = " LSP Clients ",
      title_pos = "center",
      height = 0.6,
      width = 0.8,
      preview = {
        horizontal = "right:50%",
      },
    },
    actions = {
      ["ctrl-r"] = function(selected, _)
        local parts = vim.split(selected[1], "\t")
        local client_name = parts[1]
        for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
          if client.name == client_name then
            vim.notify("Restarting LSP: " .. client_name, vim.log.levels.INFO)
            vim.cmd("LspRestart " .. client.id)
            break
          end
        end
      end,
      ["ctrl-s"] = function(selected, _)
        local parts = vim.split(selected[1], "\t")
        local client_name = parts[1]
        for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
          if client.name == client_name then
            vim.notify("Stopping LSP: " .. client_name, vim.log.levels.INFO)
            vim.cmd("LspStop " .. client.id)
            break
          end
        end
      end,
    },
  })
end, {})
