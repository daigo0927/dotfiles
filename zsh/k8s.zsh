export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

[[ $commands[kubectl] ]] && source <(kubectl completion zsh)
