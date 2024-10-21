if [ -n "$INSIDE_EMACS" ]; then
    export PS1='\[\033[32m\]\u@\h \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ '
fi

if ! emacsclient -e "(+ 1 1)" &>/dev/null; then
    emacs --bg-daemon
else
    echo "Emacs server Online"
fi

alias python3="python"
alias doom="$(cygpath -w $(which doom))"
alias er='emacsclient -e "(kill-emacs)" && emacs --bg-daemon';
alias ec="emacsclient"
alias ect="emacsclient -t"
export EDITOR="emacsclient -t"
export WORK="/c/Users/FAFI3L8/work"
export EMACS_PROFILE="work"
export APPDATA="/c/Users/FAFI3L8/AppData/Local/"
export VCFG="$appdata/nvim"
