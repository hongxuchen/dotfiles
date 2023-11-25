my_ssh() {
    ssh "$@" -t -- /bin/sh -c 'tmux has-session && exec tmux attach || exec tmux'
}

my_vim_startup_time() {
    local profile_fname=$(mktemp)
    time nvim --headless --startuptime "${profile_fname}" -c q
    less "${profile_fname}"
    rm "${profile_fname}"
}

# utility function for nvim
v () {
    if (( ${+MY_NVIM_LISTEN_ADDRESS} )); then
        if [ -S $MY_NVIM_LISTEN_ADDRESS ]; then
            if [ "$#" -lt 1 ]; then
                echo "usage: $0 \$FILE"
                return 1
            fi
            fname=$(realpath $1)
            command nvim --server "$MY_NVIM_LISTEN_ADDRESS" --remote "$fname"
            echo "Attaching existing nvim: [$fname]"
        else
            command nvim --listen "$MY_NVIM_LISTEN_ADDRESS" "$@"
        fi
    else
        echo "\$MY_NVIM_LISTEN_ADDRESS unset"
        return 2
    fi
}

my_zshreload() {
    autoload -Uz compinit zrecompile
    compinit -d "${ZSH_CACHE_DIR}/zcomp-$HOST"
    for f in "$HOME/.zshrc" "${ZSH_CACHE_DIR}/zcomp-$HOST"; do
        zrecompile -p "$f" && command rm -f "$f.zwc.old"
    done
    source "$HOME/.zshrc"
}

_str_contains() {
    string="$1"
    substring="$2"
    if [[ "${string#*$substring}" != "$string" ]]
    then
        return 0    # $substring is in $string
    else
        return 1    # $substring is not in $string
    fi
}

gcd() {
    local root rc errmsg in_gitdir gitdir not_in_git
    in_gitdir=$(git rev-parse --is-inside-git-dir 2>/dev/null)
    if [[ $in_gitdir == "true" ]]; then
        gitdir=$(realpath "$(git rev-parse --git-dir)")
        cd "$(dirname "$gitdir")"
        return 0
    fi
    tmpf=$(mktemp)
    root=$(git rev-parse --show-toplevel 2>"$tmpf")
    rc=$?
    set +x
    if [[ $rc -eq 128 ]]; then
        errmsg=$(<$tmpf)
        _str_contains "$errmsg"  "not a git repository"
        not_in_git=$?
        if [[ $not_in_git ]]; then
            echo "not in git: $PWD"
            return $rc
        else
            echo "??? $errmsg"
            return 1
        fi
    else
        cd "$root" || return 1
    fi
}

#shellcheck disable=SC2164,SC2181
my_cd_pypath () {
    if [ "$#" -lt 1 ]; then
        echo "usage: $0 file.py"
        return 1
    fi
    cd "$(python -c "import os.path as _, ${1}; \
        print(_.dirname(_.realpath(${1}.__file__[:-1])))"
)"
}

my_pypaths() {
  python -m site
}

# NOTE: "command not found" for aliases
_try_get_version() {
  res="$($1 --version 2>/dev/null)"
  if [[ $? -eq 0 ]] then
    echo $res
    return
  fi
  res="$($1 -v 2>/dev/null)"
  if [[ $? -eq 0 ]] then
    echo $res
    return
  fi
  res="$($1 -V 2>/dev/null)"
  if [[ $? -eq 0 ]] then
    echo $res
    return
  fi
  res="$($1 version 2>/dev/null)"
  if [[ $? -eq 0 ]] then
    echo $res
    return
  fi
  echo ""
}

# FIXME: compdef not work
gcm() {
  if [[ "$#" -ne 1 ]] then
    print "usage: gcm <CMD>"
    return 1
  fi
  cmd=$1

  local funcs=${(k)functions}
  funcs=(${(@s: :)funcs})
  if (($funcs[(Ie)$cmd])); then
    tmpf=$(mktemp --suffix=.zsh)
    whence -ac $cmd >$tmpf
    bat $tmpf
    return 0
  fi

  results=("${(@f)$(whence -ac $cmd)}")
  len=${#results[@]}
  i=1
  for f in $results; do
    if [[ -e $f ]]; then
      res=$(file $f)
      if [[ $len -gt 1 ]] then
        print -P "%F{yellow}$i: %F{red}$res%f"
      else
        print -P "%F{red}$res%f"
      fi
      if [[ -L $f ]]; then
        file $(realpath $f);
      fi
      version="$(_try_get_version $f)"
      if [[ -n $version ]]; then
        print "$version"
      fi
    else
      if [[ $len -gt 1 ]] then
        print -P "%F{yellow}$i: %F{red}$f%f"
      else
        print "$f"
      fi
    fi
    i=$((i+1))
  done
}
compdef gcm=whence

# `path` is an array created along with `PATH`
my_paths() {
  local fp
  local upath=()
  for fp in $path; do
    if [[ -d $fp ]]; then
      echo $fp
    else
      upath+=("$fp")
    fi
  done
  if [[ ${#upath[@]} -ne 0 ]]; then
    print -P "\n%F{red}Unkown paths%f: ${(j:, :)upath}\n"
  fi
}
