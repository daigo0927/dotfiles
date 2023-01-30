# export LANG=ja_JP.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
# vim
export XDG_CONFIG_HOME=~/.config
# less
export LESS="-Rgj10 --no-init --quit-if-one-screen --RAW-CONTROL-CHARS"
# go
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin
# rust
export PATH="$HOME/.cargo/bin:$PATH"

if [ -x "`which go`" ]; then
  export GOROOT=`go env GOROOT`
  export PATH=$PATH:$GOROOT/bin
fi

. "$HOME/.cargo/env"
