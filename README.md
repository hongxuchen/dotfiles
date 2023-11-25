Synopsis
========

My dotfile settings for Linux (Debian Sid/Ubuntu 22.10) and MacOS (13, Ventura).

LICENSE
=======
[MIT License](https://github.com/hongxuchen/dotfiles/tree/master/LICENSE).

Setup
============

Script setup
------------

Suppose the root directory of the repository is `$DOT_DIR`. The general idea is to make symlinks for the dotfiles with the setup script: `$DOT_DIR/install.py`.
  * For the files or directories starting with `_`, they will be symlinked to the corresponding dotfiles in `$HOME` directory. For example, `$DOT_DIR/_profile` will be symlinked to `$HOME/.profile`.
  * The Linux or MacOS specific dotfiles are those that start with `_` and reside in `$DOT_DIR/Linux` or `$DOT_DIR/Darwin`, respectively. For example, `$HOME/.gdbinit` is symlinked to `$DOT_DIR/Linux/_gdbinit` on a Linux machine but not on a MacOS.
  * It is suggested to *dryrun* (w/o actually running) it beforehand with `$DOT_DIR/install.py -n` (more details with `$DOT_DIR/install.py -h`) to see the effects.
  * Please close other applications (especially browsers like google-chrome, microsoft-edge, etc) during setup (especially on Linux), as the setup script will move the original `$HOME/.config` (to `$DOT_DIR/.BAK/_BAK.config` by default) before generating the symlink directory `$HOME/.config`.
  * Restore your original dotfiles by running `$DOT_DIR/install.py -r`.


  ```bash
  # typical flows for setup

  # clone with git ssh
  # this means that for FIRST TIME setup, `git` should be installed beforehand
  git clone --recursive git@github.com:hongxuchen/dotfiles.git $DOT_DIR
  # or with https
  git clone --recursive https: https://github.com/hongxuchen/dotfiles.git

  cd $DOT_DIR

  # dryrun to see whether there are dangerous operations
  ./install.py -n

  ## close other applications for a smooth setup and install
  ./install.py

  # see further uses of this setup script
  ./install.py -h
  ```

System relevant installations
-----------------------------

Packages that can be installed via system package manager:

* Both:

```bash
cd misc/pkgs
cargo install $(cat cargo)
pip install -U -r requirements.txt 
# or install locally if you'd like to only affect yourself (and maybe have no permissions)
pip install --user -U -r requirements.txt 
```

* Linux:
Most of them can be installed via `apt`:
```bash
sudo apt install $(cat Linux/debs)
```

Other desktop packages can be installed via [flatpak](https://flathub.org/home); **never** use [snap](https://snapcraft.io/)
  * Add a Chinese source mirror of flathub if you are in China:
```bash
flatpak remote-add --if-not-exists sjtu  https://mirror.sjtu.edu.cn/flathub/flathub.flatpakrepo
flatpak remotes
```
  * Packages: `telegram`

* Darwin (with [homebrew](https://brew.sh/)):

> For Chinese users, it is suggested to use a source mirror for quick installation. I personally follow [tuna](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew/) source for both fresh installation and updates.

```bash
brew install --cask $(cat Darwin/casks) 
brew install --formula $(cat Darwin/formulas)
```

Misc installations
------------------

* Some of the configurations/binaries cannot be installed by the setup script, please update accordingly.
  * [neovim](https://github.com/neovim/neovim/releases/tag/stable) -- I use the latest (nightly) version
  * [rustup](https://rustup.rs/) -- don't use debian/brew cargo/rustc, but always use the rustup one
  * [microsoft-edge](https://www.microsoft.com/en-us/edge/download?form=MA13FJ) -- In China, Edge is better
  * Clash -- for the well-known reason in mainland China
    * Linux: [Clash CLI](https://github.com/Dreamacro/clash/releases)
    * MacOS: [ClashX](https://github.com/yichengchen/clashX/releases)
  * [v2raya](https://v2raya.org/docs/prologue/installation/) -- for the well-known reason in mainland China
  * [leanprover](https://github.com/leanprover/lean4-nightly/releases) -- I'd like to learn some assisted proving

* Despite that my configurations can be for general purpose, some "source mirrors", like `$HOME/.pip/pip.conf`, `$HOME/.npmrc`, are configured with Chinese sources, for better network downloading speed.

* Use [Nerd Fonts](https://www.nerdfonts.com/) for nice icon display.
    - Download into `$HOME/.fonts` and use `fc-cache -fv` to update it;
    - Test the UTF-8 support with `misc/UTF-8-demo.txt`;
    - Test the icons with neovim;
    - on Debian derivatives, make sure urxvt is from `rxvt-unicode-256color`;
    - xterm is not recommended as it has poor support for unicode characters.


Settings
========

Fundamentals
------------

1. **neovim**(`$HOME/.config/nvim/`)
The setup for neovim is done by [lazy.nvim](https://github.com/folke/lazy.nvim). Just type `nvim` and plugins will be installed automatically. DO keep in mind to install nvim plugins via a good network that can access GitHub smoothly.
My five cents for using neovim:
    * Use as fewer plugins as possible, unless the plugin brings significant efficiency;
    * Keep `treesitter`, `LSP`, `Mason` installed packages, as updated as possible;
    * Use the `version="*"` in `lazy.nvim` to always install the latest plugins;
    * Go to GitHub issue pages to find out the answers when there are errors.
    * Not using [helix](https://helix-editor.com/) since I've customized to Vim keymaps; VSCode and Intellij are still used.

I have also aliased `vim`/`vi` to use nvim with a simple configuration, [simple.lua](https://github.com/hongxuchen/dotfiles/blob/master/_config/nvim/simple.lua), which does not require any installations of plugins.

1. [zsh](http://www.zsh.org/)(derived from [oh-my-zsh](https://github.com/ohmyzsh/ohmyzsh))

    - relevant files:
        - `$HOME/.zshenv` -- for all zsh uses
        - `$HOME/.zprofile` -- for zsh login shell use
        - `$HOME/.zshrc` -- for zsh interactive shell use
        - `$HOME/.aliases` -- sourced inside $HOME/.zshrc, for aliasing
        - `$HOME/.omz/` -- sourced inside $HOME/.zshrc, for oh-my-zsh style zsh uses

1. [**ipython**](http://ipython.org/)(especially for [sh](http://ipython.org/ipython-doc/dev/interactive/shell.html)/[nb](http://ipython.org/notebook.html) profile)(`$HOME/.ipython/`)

1. **git**(`$HOME/.gitconfig`)

    - [git-extras](https://github.com/tj/git-extras) is awesome

1. [**tmux**](http://tmux.sourceforge.net/)(`$HOME/.tmux.conf`)
    * [zellij](https://zellij.dev/) is not used since I've been used to tmux
    * [GNU screen](https://www.gnu.org/software/screen/) is way too old

Linux Specific
--------------

They are *not* exactly Linux only, but *I* tend not to use them on MacOS. They lie in [Linux/](https://github.com/hongxuchen/dotfiles/tree/master/Linux). 

- **gdb**(`$HOME/.gdbinit`, `$HOME/.gdb`)
    - gdb is superseded by [lldb](http://lldb.llvm.org) on newer MacOS.

- **X11** settings
    - `$HOME/.Xresources`: for XTerm, URxvt, should run `xrdb -load $HOME/.Xresources` firstly
    - `$HOME/.xscreensaver`

- **wget**($HOME/.wgetrc)

MacOS Specific
--------------
The MacOS specific dotfiles lie in [Darwin/](https://github.com/hongxuchen/dotfiles/tree/master/Darwin) directory.

- [**slate**](https://github.com/jigish/slate)(`$HOME/.slate`)

TODOs
=====
* add local/global machine-specific settings
