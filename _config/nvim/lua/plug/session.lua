return {
  "folke/persistence.nvim",
  -- keys = { "<leader>pd", "<leader>pl" },
  config = function()
    local p = require("persistence")
    p.setup()
    local u = require("core.utils")
    -- for session management, for laziness only required when invoked
    u.keymap("n", "<leader>pd", function()
      p.load()
    end, u.opts, "[persistence] restore curdir")
    u.keymap("n", "<leader>pl", function()
      p.load { last = true }
    end, u.opts, "[persistence] restore last")
    u.keymap("n", "<leader>ps", function()
      p.start()
    end, u.opts, "[persistence] start")
    u.keymap("n", "<leader>pt", function()
      p.stop()
    end, u.opts, "[persistence] stop")
  end,
}
