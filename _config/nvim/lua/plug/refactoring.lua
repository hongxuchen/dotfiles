return {
  "ThePrimeagen/refactoring.nvim",
  keys = {
    {
      "<localleader>rr",
      function()
        require('telescope').extensions.refactoring.refactors()
      end,
      mode = {"x", "n"},
      desc = "[refactor] choose refactoring",
    },
  },
  config = function()
    require("telescope").load_extension("refactoring")
    require("refactoring").setup {
      prompt_func_return_type = {
        go = false,
        java = false,

        cpp = false,
        c = false,
        h = false,
        hpp = false,
        cxx = false,
      },
      prompt_func_param_type = {
        go = false,
        java = false,

        cpp = false,
        c = false,
        h = false,
        hpp = false,
        cxx = false,
      },
      printf_statements = {},
      print_var_statements = {},
    }
  end,
}
