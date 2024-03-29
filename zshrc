# compile if .zwc is old
if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
   echo "zcompile .zshrc"
   zcompile ~/.zshrc
fi
if [ ~/.zshenv -nt ~/.zshenv.zwc ]; then
   echo "zcompile .zshenv"
   zcompile ~/.zshenv
fi
ZSH_HOME=~/.zsh/
for f (${ZSH_HOME}*.zsh) {
  if [ $f -nt $f.zwc ]; then
    echo "zcompile $f"
    zcompile $f
  fi
}

# OS dependent
if [ "$(uname)" = "Darwin" ]; then
  source ${ZSH_HOME}darwin.zsh
else # Linux
  source ${ZSH_HOME}linux.zsh
  source ${ZSH_HOME}cuda.zsh
fi

# general
source ${ZSH_HOME}zplug.zsh
source ${ZSH_HOME}basic.zsh
source ${ZSH_HOME}commands.zsh
source ${ZSH_HOME}aliases.zsh
source ${ZSH_HOME}python.zsh
source ${ZSH_HOME}emacs.zsh
source ${ZSH_HOME}ruby.zsh
source ${ZSH_HOME}go.zsh
source ${ZSH_HOME}k8s.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

if [ -e "${HOME}/.nvm" ]; then
   export NVM_DIR="$HOME/.nvm"
     [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
     [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
fi


# The next line updates PATH for the Google Cloud SDK.
if [ -f "${HOME}/google-cloud-sdk/path.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "${HOME}/google-cloud-sdk/completion.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/completion.zsh.inc"; fi

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/dhirooka/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)