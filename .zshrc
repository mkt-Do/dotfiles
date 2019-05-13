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
eval `tset -s xterm-24bits`

# Alias
alias transen='trans -b -sl=en -tl=ja'
# alias e='emacs'
alias ed='env TERM=xterm-256color emacs --daemon'
alias e='emacsclient -nw'
alias kille='emacsclient -e "(kill-emacs)"'
