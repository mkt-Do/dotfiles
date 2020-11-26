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
