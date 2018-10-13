
sh ~/.fehbg

alias ..="cd .."

alias ll="ls -la"
alias ls="ls --color=auto"
alias grep="grep --color=auto"

# Production
alias gccp="gcc -Werror -O2"
# Development
alias gccd="gcc -Wall -Wextra -Wpedantic -Wduplicated-cond -Wduplicated-branches \
-Wlogical-op -Wnull-dereference -Wshadow=local -Og"
