#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
alias e="emacsclient -nw"
alias ed="env TERM=xterm-256color emacs --daemon"
alias ke="emacsclient -e '(kill-emacs)'"
alias re="ke | ed"
alias catn="bat"
alias date="gdate"
alias sed="gsed"
alias ln='gln'
alias rust-repl='evcxr'

# EN -> JP
function ejdict() {
  grep "$1" /Users/makoto.ishizaki/dict/gene-utf8.txt -E -A 1 -wi --color=always | less -R -FX
}

function jedict() {
  grep "$1" /Users/makoto.ishizaki/dict/gene-utf8.txt -E -B 1 -wi --color=always | less -R -FX
}

# PATH
# Scala 3
PATH="$PATH:/Users/makoto.ishizaki/Library/Application Support/Coursier/bin"
## nodenv
export PATH="$HOME/.nodenv/bin:$PATH"
eval "$(nodenv init -)"
# JAVA_HOME
# jenv
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
#export JAVA_HOME="$(/usr/libexec/java_home)"
#export PATH="${JAVA_HOME}/bin:$PATH"
# pyenv & virtualenv
export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
# OCaml
eval "$(eval $(opam env))"
# Rust
export PATH="$HOME/.cargo/bin:$PATH"
# Flutter
export PATH="$HOME/flutter/bin:$PATH"
# Golang
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
export GO111MODULE=on
eval "$(goenv init -)"
export PATH="$GOROOT/bin:$PATH"
export GOPATH="$HOME/.go"
export PATH="$PATH:$GOPATH/bin"

# zsh-bd
. $HOME/.zsh/plugins/bd/bd.zsh
