return {
  "akinsho/toggleterm.nvim",
  cmd = {
    "ToggleTerm",
    "ToggleTermToggleAll",
    "ToggleTermSendCurrentLine",
    "ToggleTermSendVisualLines",
    "ToggleTermSendVisualSelection",
  },
  keys = {
    {
      "<leader>sp",
      function()
        local Terminal = require("toggleterm.terminal").Terminal
        local ipython = Terminal:new { cmd = "ipython", hidden = false }
        ipython:toggle()
      end,
      desc = "[Term] ipython",
    },
    {
      "<leader>sd",
      function()
        local Terminal = require("toggleterm.terminal").Terminal
        local shell = Terminal:new { cmd = vim.env.SHELL, hidden = false }
        shell:toggle()
      end,
      desc = "[Term] default shell",
    },
    { "<leader>ss", "<Cmd>TermSelect<CR>", desc = "[Term] TermSelect" },
    { "<leader>se", ":TermExec cmd=''", desc = "[Term] TermExec" },
    { "<leader>st", "<Cmd>ToggleTerm<CR>", desc = "[Term] ToggleTerm" },
    { "<leader>sa", "<Cmd>ToggleTermToggleAll<CR>", desc = "[Term] ToggleTermToggleAll" },
  },
  config = function()
    require("toggleterm").setup { direction = "tab" }
  end,
}
