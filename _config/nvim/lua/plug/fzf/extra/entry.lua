local curdir = debug.getinfo(1, 'S').source:sub(2):match('(.*/)')
return vim
  .iter(vim.fs.dir(vim.fs.joinpath(curdir, 'providers')))
  :fold({}, function(M, name)
    name = name:match('(.*)%.lua$')
    local mod = 'plug.fzf.extra.providers.' .. name
    M[name] = function(...)
      require('fzf-lua').set_info { mod = mod, cmd = name, fnc = name }
      return require(mod)(...)
    end
    return M
  end)
