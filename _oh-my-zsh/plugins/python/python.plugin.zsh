alias pyfind='find . -name "*.py"'
alias pygrep='grep --include="*.py"'

function pyclean() {
    ZSH_PYCLEAN_PLACES=${*:-'.'}
    find ${ZSH_PYCLEAN_PLACES} -type f -name "*.py[co]" -delete
}
