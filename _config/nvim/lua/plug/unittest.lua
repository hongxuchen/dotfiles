-- TODO: https://www.reddit.com/r/neovim/comments/108nc7g/introducing_continuoustestingnvim/
-- NOTE: neotest-rust is NOT needed since we have LSP codelens
return {
  "nvim-neotest/neotest",
  -- keys = {"<localleader>uu", "<localleader>ud", "<localleader>uf"},
  lazy = true,
  dependencies = {
    "nvim-neotest/neotest-plenary",
    "nvim-neotest/neotest-python",
  },
  config = function()
    local t = require("neotest")
    t.setup {
      adapters = {
        require("neotest-python"),
        require("neotest-plenary"),
      },
    }
    local u = require("core.utils")
    u.keymap("n", "<localleader>ua", function()
      t.run.attach()
    end, u.opts, "[neotest] attach test")
    u.keymap("n", "<localleader>ud", function()
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
