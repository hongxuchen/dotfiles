return {
  {
    "nvim-telescope/telescope.nvim",
    event = "VeryLazy",
    dependencies = {
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make", cond = require("core.utils").not_win },
      { "nvim-telescope/telescope-symbols.nvim" },
      { "jvgrootveld/telescope-zoxide" },
      { "tsakirist/telescope-lazy.nvim" },
    },
    config = function()
      local u = require("core.utils")
      local t = require("telescope")
      local builtin = require("telescope.builtin")
      local actions = require("telescope.actions")

      local vimgrep_arguments = {
        "rg",
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--smart-case",
        "--trim",
        "--hidden",
        "--glob",
        "!**/.git/*",
      }

      local lsp_jumper_conf = {
        show_line = false,
        fname_width = 30,
        path_display = { shorten = { len = 1, exclude = { -1, -2, -3 } } },
      }

      local lsp_symbol_conf = {
        show_line = false,
      }

      local file_conf = {
        previewer = false,
        disable_devicons = true,
      }

      local grep_str_conf = {
        disable_coordinates = true,
        disable_devicons = true,
      }

      t.setup {
        defaults = {
          layout_strategy = "horizontal",
          sorting_strategy = "ascending",
          layout_config = {
            horizontal = {
              prompt_position = "top",
            },
          },
          preview = {
            check_mime_type = true,
            filesize_limit = require("core.utils").SizeInMegaBytes,
            timeout = 100,
            treesitter = false,
            msg_bg_fillchar = "/",
            hide_on_startup = false,
          },
          color_devicons = false,
          history = false,
          wrap_results = false,
          vimgrep_arguments = vimgrep_arguments,
          prompt_prefix = " ",
          selection_caret = " ",
          path_display = { "truncate" },
          mappings = {
            i = {
              ["PageUp"] = false,
              ["PageDown"] = false,
              ["<M-n>"] = actions.results_scrolling_down,
              ["<M-p>"] = actions.results_scrolling_up,
            },
          },
        },
        pickers = {
          -- file relevant
          find_files = vim.tbl_extend("force", file_conf, {
            no_ignore = false,
            no_ignore_parent = false,
            follow = false,
          }),
          oldfiles = file_conf,
          git_files = vim.tbl_extend("force", file_conf, {
            show_untracked = true,
            use_git_root = true,
          }),
          buffers = vim.tbl_extend("force", file_conf, {
            ignore_current_buffer = true,
          }),
          -- string relevant
          live_grep = grep_str_conf,
          grep_string = vim.tbl_extend("force", grep_str_conf, {
            use_regex = true,
            only_sort_text = true,
          }),
          -- search in current buffer
          current_buffer_fuzzy_find = {
            skip_empty_lines = true,
          },
          -- lsp relevant
          lsp_definitions = lsp_jumper_conf,
          lsp_references = vim.tbl_extend("force", lsp_jumper_conf, {
            include_declaration = false,
          }),
          lsp_type_definitions = lsp_jumper_conf,
          lsp_implementations = lsp_jumper_conf,
          lsp_incoming_calls = lsp_jumper_conf,
          lsp_outgoing_calls = lsp_jumper_conf,
          lsp_document_symbols = lsp_symbol_conf,
          lsp_workspace_symbols = lsp_symbol_conf,
          lsp_dynamic_workspace_symbols = lsp_symbol_conf,
          -- diagnostics
          diagnostics = {
            no_sign = true,
          },
          -- misc
          jumplist = {
            show_line = false,
          },
          planets = {
            show_pluto = true,
            show_moon = true,
          },
        },
        symbols = { "emoji", "gitmoji", "math", "latex" },
        extensions = {
          fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
          },
          zoxide = {
            mappings = {
              ["<CR>"] = {
                keepinsert = true,
                action = function(selection)
                  builtin.find_files { cwd = selection.path }
                end,
              },
            },
          },
        },
      }

      if u.not_win then
        t.load_extension("fzf") -- fzf native sorting algorithm
      end
      t.load_extension("zoxide") -- zoxide extension
      t.load_extension("lazy") -- lazy plugin navigation

      u.keymap("n", "<leader>ll", "<Cmd>Telescope lazy<CR>", u.opts, "[telescope] navigate to lazy plugins")
      u.keymap("n", "<leader>ss", "<Cmd>Telescope symbols<CR>", u.opts, "[telescope] complete symbols")
      u.keymap("n", "<leader>z", t.extensions.zoxide.list, u.opts, "[telescope] search recent directories")
    end,
  },
}
