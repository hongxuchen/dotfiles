return {
  "nvim-neotest/neotest",
  keys = {
    {
      "<localleader>ua",
      function()
        require("neotest").run.attach()
      end,
      desc = "[neotest] attach test",
    },
    {
      "<localleader>uu",
      function()
        require("neotest").run.run()
      end,
      desc = "[neotest] nearest test",
    },
    {
      "<localleader>us",
      function()
        require("neotest").run.stop()
      end,
      desc = "[neotest] stop test",
    },
    {
      "<localleader>uf",
      function()
        local fname = vim.api.nvim_buf_get_name(0)
        require("neotest").run.run(fname)
      end,
      desc = "[neotest] file test",
    },
    {
      "<localleader>ud",
      function()
        ---@diagnostic disable-next-line: missing-fields
        require("neotest").run.run { vim.api.nvim_buf_get_name(0), strategy = "dap" }
      end,
      desc = "[neotest] debug test",
    },
  },
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
  end,
}
