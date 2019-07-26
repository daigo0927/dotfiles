if [ -x "`which pip`" ]; then
  function _pip_completion {
    local words cword
    read -Ac words
    read -cn cword
    reply=( $( COMP_WORDS="$words[*]" \
               COMP_CWORD=$(( cword-1 )) \
               PIP_AUTO_COMPLETE=1 $words[1] ) )
  }
  compctl -K _pip_completion pip
fi

# aliases
alias python="python3"
alias python2="python2"
alias py="python3"
alias j="jupyter"

