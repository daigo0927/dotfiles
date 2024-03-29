export PYENV_ROOT="$HOME/.pyenv"
if [ -d ${PYENV_ROOT} ]; then
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
  if [ -d ${PYENV_ROOT}/plugins/pyenv-virtualenv ]; then
    eval "$(pyenv virtualenv-init -)"
  fi
fi

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

# poetry path
export PATH="$HOME/.poetry/bin:$PATH"

# aliases
alias py="python"
alias j="jupyter"

