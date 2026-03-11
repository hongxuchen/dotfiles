---@type LazyPluginSpec
return {
  "stevearc/overseer.nvim",
  opts = function()
    return {
      dap = false,
      form = {
        border = "rounded",
      },
      confirm = {
        border = "rounded",
      },
      task_win = {
        border = "rounded",
      },
      component_aliases = {
        default = {
          "on_exit_set_status",
          "on_complete_notify",
          "on_complete_dispose",
          "unique",
        },
      },
    }
  end,
  config = function(_, opts)
    local overseer = require("overseer")

    overseer.setup(opts)

    local templates = {
      {
        name = "C++ build single",
        builder = function()
          return {
            cmd = { "clang++" },
            args = {
              "-g",
              "-std=c++17",
              vim.fn.expand("%:p"),
              "-o",
              vim.fn.expand("%:p:t:r") .. ".out",
            },
          }
        end,
        condition = {
          filetype = { "cpp" },
        },
      },
    }
    for _, template in ipairs(templates) do
      overseer.register_template(template)
    end
  end,
  keys = {
    { "<leader>sr", "<cmd>OverseerRun<CR>", desc = "[overseer] Run" },
    { "<leader>sl", "<cmd>OverseerToggle<CR>", desc = "[overseer] List" },
    {
      "<leader>sn",
      function()
        require("overseer").run_task { autostart = false }
      end,
      desc = "[overseer] New",
    },
    { "<leader>sa", "<cmd>OverseerTaskAction<CR>", desc = "[overseer] Action" },
    {
      "<leader>si",
      function()
        vim.cmd("checkhealth overseer")
      end,
      desc = "[overseer] Info",
    },
    {
      "<leader>sc",
      function()
        require("overseer").clear_task_cache()
      end,
      desc = "[overseer] Clear cache",
    },
  },
}
