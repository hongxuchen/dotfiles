-- TODO: check https://github.com/Civitasv/runvim
return {
  "mfussenegger/nvim-dap",
  dependencies = {
    "rcarriga/nvim-dap-ui",
    "theHamsta/nvim-dap-virtual-text",
  },
  keys = { "<localleader>Db" },
  config = function()
    local u = require("core.utils")

    vim.fn.sign_define("DapBreakpoint", { text = "🛑", texthl = "", linehl = "", numhl = "" })
    vim.fn.sign_define("DapStopped", { text = "⭐️", texthl = "", linehl = "", numhl = "" })

    local dap = require("dap")
    local dapui = require("dapui")
    local dap_vt = require("nvim-dap-virtual-text")

    ---@diagnostic disable-next-line: missing-fields
    dap_vt.setup {}
    ---@diagnostic disable-next-line: missing-fields
    dapui.setup {}

    -- automatically setup dap ui
    dap.listeners.after.event_initialized["dapui_config"] = function()
      dapui.open {}
    end
    dap.listeners.before.event_terminated["dapui_config"] = function()
      dapui.close {}
    end
    dap.listeners.before.event_exited["dapui_config"] = function()
      dapui.close {}
    end

    u.au("Filetype", {
      group = u.myAutoGroup,
      pattern = "dap-repl",
      callback = function()
        require("dap.ext.autocompl").attach()
      end,
    })
    u.keymap("n", "<localleader>Db", dap.toggle_breakpoint, u.opts, "[dap] toggle breakpoints")
    u.keymap("n", "<localleader>Dc", dap.continue, u.opts, "[dap] continue")
    u.keymap("n", "<localleader>De", dapui.eval, u.opts, "[dap] eval")
    u.keymap("n", "<localleader>Di", dap.step_into, u.opts, "[dap] step info")
    u.keymap("n", "<localleader>Do", dap.step_out, u.opts, "[dap] step out")
    u.keymap("n", "<localleader>DO", dap.step_over, u.opts, "[dap] step over")
    u.keymap("n", "<localleader>Dt", dap.terminate, u.opts, "[dap] terminate")

    --- adapters and configurations
    -- for python
    dap.adapters["python"] = {
      type = "executable",
      command = "python",
      args = { "-m", "debugpy.adapter" },
    }
    dap.configurations["python"] = {
      {
        type = "python",
        request = "launch",
        name = "Launch file",
        program = "${file}",
        pythonPath = function()
          return "/usr/bin/python"
        end,
      },
    }

    -- for c/cpp/rust
    dap.adapters["cpp"] = {
      type = "executable",
      attach = {
        pidProperty = "pid",
        pidSelect = "ask",
      },
      command = "lldb-vscode-14",
      env = {
        LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES",
      },
      name = "lldb",
    }
    dap.configurations["cpp"] = {
      {
        name = "lldb",
        type = "cpp",
        request = "launch",
        cwd = "${workspaceFolder}",
        program = function()
          local res
          vim.ui.input(
            { prompt = "Path to executable: ", default = vim.fn.cwd() .. "/", completion = "file" },
            function(input)
              res = input
            end
          )
          return res
        end,
        -- externalTerminal = false,
        stopOnEntry = false,
        args = {},
      },
    }
    dap.configurations["c"] = dap.configurations["cpp"]
    dap.configurations["rust"] = dap.configurations["cpp"]
  end,
}
