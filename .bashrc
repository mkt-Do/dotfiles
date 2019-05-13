export LANG=ja_JP.UTF-8

case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${PWD##*/}\007"'
    show_command_in_title_bar()
    {
        case "$BASH_COMMAND" in
            *\033]0*)
                ;;
            *)
                echo -ne "\033]0;${BASH_COMMAND} - ${PWD##*/}\007"
                ;;
        esac
    }
    trap show_command_in_title_bar DEBUG
    ;;
*)
    ;;
esac
case "${OSTYPE}" in
darwin*)
    alias ls="ls -G"
    alias ll="ls -lG"
    alias la="ls -laG"
    ;;
linux*)
    alias ls='ls --color'
    alias ll='ls -l --color'
    alias la='la -la --color'
    ;;
esac


PATH="/Users/makoto-ishizaki/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/makoto-ishizaki/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/makoto-ishizaki/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/makoto-ishizaki/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/makoto-ishizaki/perl5"; export PERL_MM_OPT;

export PATH="/Users/makoto-ishizaki/.phpenv/bin:$PATH"
eval "$(phpenv init -)"
