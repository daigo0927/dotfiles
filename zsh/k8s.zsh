export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

alias k="kubectl"
source "/opt/homebrew/opt/kube-ps1/share/kube-ps1.sh"
PS1='$(kube_ps1)'$PS1
