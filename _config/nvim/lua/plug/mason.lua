return {
  {
    "williamboman/mason.nvim",
    cmd = { "Mason", "MasonPkg" },
    config = function()
      require("mason").setup()
      local registry = require "mason-registry"

      -- Refresh registry on setup
      registry.refresh()

      -- Helper function to format list
      local function format_list(label, items, empty_label)
        if not items or #items == 0 then
          return { string.format("  %s: %s", label, empty_label or "N/A") }
        end
        local formatted = { string.format("  %s:", label) }
        for _, item in ipairs(items) do
          table.insert(formatted, "      - " .. item)
        end
        return formatted
      end

      -- Helper function to sanitize and truncate strings
      local function sanitize(str)
        if not str then
          return ""
        end
        -- Replace newlines with spaces and trim
        return (str:gsub("\n", " "):gsub("\r", " "):gsub("%s+", " "):match("^%s*(.-)%s*$") or "")
      end

      local function format_desc(desc)
        if not desc or desc == "" then
          return "N/A"
        end
        desc = sanitize(desc)
        -- Truncate long descriptions to terminal width - padding
        local max_width = vim.o.columns - 40
        if #desc > max_width then
          return desc:sub(1, max_width) .. "..."
        end
        return desc
      end

      -- Package info command
      vim.api.nvim_create_user_command("MasonPkg", function(opts)
        local package_name = opts.args
        if package_name == "" then
          vim.notify("[MasonPkg] Usage: MasonPkg <package-name>", vim.log.levels.WARN)
          return
        end

        -- Wait for registry to be ready
        if not registry.is_installed("") then
          registry.refresh()
        end

        local ok, pkg = pcall(registry.get_package, package_name)
        if not ok then
          vim.notify(
            string.format("[MasonPkg] Package '%s' not found.\nTip: Use :Mason to browse available packages.", package_name),
            vim.log.levels.ERROR
          )
          return
        end

        local spec = pkg.spec
        if not spec then
          vim.notify("[MasonPkg] Failed to get package spec", vim.log.levels.ERROR)
          return
        end

        local is_installed = pkg:is_installed()
        local is_installing = pkg:is_installing()
        local installed_version = pkg:get_installed_version()
        local latest_version = pkg:get_latest_version()
        local aliases = pkg:get_aliases()

        -- Build info lines
        local lines = {
          string.format("Name:        %s", sanitize(spec.name)),
          string.format("Status:      %s", is_installing and "Installing..." or (is_installed and "Installed" or "Not Installed")),
        }

        if is_installed and installed_version then
          table.insert(lines, string.format("Version:     %s", sanitize(installed_version)))
        elseif latest_version then
          table.insert(lines, string.format("Version:     %s (latest)", sanitize(latest_version)))
        end

        if spec.description then
          table.insert(lines, string.format("Description: %s", format_desc(spec.description)))
        end

        if spec.homepage then
          table.insert(lines, string.format("Homepage:    %s", sanitize(spec.homepage)))
        end

        if spec.categories and #spec.categories > 0 then
          local cats = vim.tbl_map(sanitize, spec.categories)
          table.insert(lines, string.format("Categories:  %s", table.concat(cats, ", ")))
        end

        if spec.languages and #spec.languages > 0 then
          local langs = vim.tbl_map(sanitize, spec.languages)
          table.insert(lines, string.format("Languages:   %s", table.concat(langs, ", ")))
        end

        if aliases and #aliases > 0 then
          local als = vim.tbl_map(sanitize, aliases)
          table.insert(lines, string.format("Aliases:     %s", table.concat(als, ", ")))
        end

        if spec.deprecation then
          table.insert(lines, string.format("Deprecated:  since %s", sanitize(spec.deprecation.since or "Unknown")))
        end

        -- Display in floating window
        local buf = vim.api.nvim_create_buf(false, true)
        vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
        vim.api.nvim_buf_set_option(buf, "modifiable", false)
        vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")

        -- Calculate window size
        local width = 0
        for _, line in ipairs(lines) do
          if #line > width then
            width = #line
          end
        end
        width = math.min(width + 4, vim.o.columns - 4)
        local height = #lines + 2

        local row = math.floor((vim.o.lines - height) / 2)
        local col = math.floor((vim.o.columns - width) / 2)

        local win = vim.api.nvim_open_win(buf, true, {
          relative = "editor",
          row = row,
          col = col,
          width = width,
          height = height,
          style = "minimal",
          border = "single",
        })

        -- Close on q or <esc>
        vim.api.nvim_buf_set_keymap(buf, "n", "q", ":close<CR>", { silent = true })
        vim.api.nvim_buf_set_keymap(buf, "n", "<ESC>", ":close<CR>", { silent = true })
      end, {
        nargs = 1,
        complete = function(arg_lead)
          local all_names = registry.get_all_package_names()
          table.sort(all_names)
          return vim.tbl_filter(function(name)
            return vim.startswith(name, arg_lead)
          end, all_names)
        end,
        desc = "Show Mason package information",
      })
    end,
  },

  {
    "zapling/mason-conform.nvim",
    config = function()
      require("mason-conform").setup {
        -- Disable auto-install, use system packages instead
        ignore_install = {
          "clang-format",
          "beautysh",
          "ruff",
        },
      }
    end,
  },

  {
    "rshkarin/mason-nvim-lint",
    config = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
      require("mason-nvim-lint").setup {
        ignore_install = {
          "sqruff",
          "zsh",
          "commitlint",
        },
        automatic_installation = false,
      }
    end,
  },

  {
    "jayp0521/mason-nvim-dap.nvim",
    event = "VeryLazy",
    config = function()
      require("mason-nvim-dap").setup {
        ensure_installed = {},
        automatic_installation = false,
      }
    end,
  },

  {
    "williamboman/mason-lspconfig.nvim",
    lazy = false,
    config = function()
      require("mason-lspconfig").setup {
        ensure_installed = {
          "jsonls",
          "neocmake",
          "bashls",
          "lua_ls",
          "ts_ls",
          "vimls",
          "yamlls",
          "taplo",
          "jdtls",
        },
        -- Disable auto-install for servers that may fail
        automatic_installation = false,
      }
    end,
  },
}
