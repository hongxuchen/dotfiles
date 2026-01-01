-- Only enable if ANTHROPIC_API_KEY is set
if vim.env.ANTHROPIC_API_KEY then
  return {
    {
      "olimorris/codecompanion.nvim",
      dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-treesitter/nvim-treesitter",
      },
      opts = {
        strategies = {
          chat = {
            adapter = "anthropic",
          },
          inline = {
            adapter = "anthropic",
          },
          agent = {
            adapter = "anthropic",
          },
        },
        display = {
          chat = {
            show_settings = false,
            show_references = true,
          },
        },
        adapters = {
          anthropic = function()
            return require("codecompanion.adapters").extend("anthropic", {
              env = {
                api_key = "ANTHROPIC_API_KEY",
              },
              schema = {
                model = {
                  default = "claude-3.5-sonnet",
                },
              },
            })
          end,
        },
        opts = {
          log_level = "WARN",
        },
      },
      keys = {
        {
          "<LocalLeader>a",
          "<Cmd>CodeCompanion Chat<CR>",
          mode = { "n", "v" },
          desc = "[CodeCompanion] Open chat buffer",
        },
        {
          "<LocalLeader>i",
          "<Cmd>CodeCompanion Inline<CR>",
          mode = { "n", "v" },
          desc = "[CodeCompanion] Inline assistant",
        },
        {
          "<LocalLeader>ac",
          "<Cmd>CodeCompanion Add<CR>",
          mode = "v",
          desc = "[CodeCompanion] Add to chat",
        },
      },
      config = function(_, opts)
        require("codecompanion").setup(opts)
      end,
    },
  }
else
  return {} -- Disabled
end
