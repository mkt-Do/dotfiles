if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

export PATH=$HOME/.nodebrew/current/bin:$PATH

# jenv
export JENV_ROOT="$HOME/.jenv"
#if [ -d "${JENV_ROOT}" ]; then
  export PATH="$JENV_ROOT/bin:$PATH"
#  eval "$(jenv init -)"
#fi

# goenv
export PATH="$HOME/.goenv/bin:$PATH"
eval "$(goenv init -)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/makoto-ishizaki/.sdkman"
[[ -s "/Users/makoto-ishizaki/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/makoto-ishizaki/.sdkman/bin/sdkman-init.sh"

alias transen='trans -b -sl=en -tl=ja'
alias e='emacs'
