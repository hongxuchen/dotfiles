return {
  "nvim-neotest/neotest",
  keys = {"<localleader>uu", "<localleader>ud", "<localleader>uf"},
  lazy = true,
  dependencies = {
    "nvim-neotest/neotest-plenary",
    "nvim-neotest/neotest-python",
    "alfaix/neotest-gtest",
    "mrcjkb/rustaceanvim",
  },
  config = function()
    local t = require("neotest")
    ---@diagnostic disable-next-line: missing-fields
    t.setup {
      adapters = {
        require("neotest-python"),
        require("neotest-plenary"),
        require("rustaceanvim.neotest"),
        require("neotest-gtest").setup {},
      },
    }
    local u = require("core.utils")
    u.keymap("n", "<localleader>ua", function()
      t.run.attach()
    end, u.opts, "[neotest] attach test")
    u.keymap("n", "<localleader>ud", function()
      ---@diagnostic disable-next-line: missing-fields
      t.run.run { strategy = "dap" }
    end, u.opts, "[neotest] nearest debug test")
    u.keymap("n", "<localleader>uf", function()
      local fname = vim.api.nvim_buf_get_name(0)
      t.run.run(fname)
      u.keymap("n", "<localleader>uu", function()
        t.run.run()
      end, u.opts, "[neotest] nearest test")
    end, u.opts, "[neotest] file test")
    u.keymap("n", "<localleader>us", function()
      t.run.stop()
    end, u.opts, "[neotest] stop test")
  end,
}
