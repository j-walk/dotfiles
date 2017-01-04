autoload -Uz compinit promptinit colors
colors
compinit
promptinit

function hs() {
  printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' -
}

PATH=$PATH:~/.cabal/bin
PATH=$PATH:~/.gem/ruby/2.3.0/bin
PATH=$PATH:~/.cargo/bin
PATH=$PATH:~/.cargo/env
GOPATH=~/.gopath



if [ "$TERM" = "screen-256color" ]
then
  PROMPT=$fg[grey]$fg[red]%c$fg[white]::
else
  PROMPT=$fg[grey]$fg[red]%c$fg[white]$
fi

if [ -e /usr/bin/vimx ]; then alias vim='/usr/bin/vimx'; fi



export alias ll='ls -l'
# unregister broken GHC packages. Run this a few times to resolve dependency rot in installed packages.
# ghc-pkg-clean -f cabal/dev/packages*.conf also works.
function ghc-pkg-clean() {
    for p in `ghc-pkg check $* 2>&1  | grep problems | awk '{print $6}' | sed -e 's/:$//'`
    do
        echo unregistering $p; ghc-pkg $* unregister $p
    done
}

# remove all installed GHC/cabal packages, leaving ~/.cabal binaries and docs in place.
# When all else fails, use this to get out of dependency hell and start over.
function ghc-pkg-reset() {
    read -p 'erasing all your user ghc and cabal packages - are you sure (y/n) ? ' ans
    test x$ans == xy && ( \
        echo 'erasing directories under ~/.ghc'; rm -rf `find ~/.ghc -maxdepth 1 -type d`; \
        echo 'erasing ~/.cabal/lib'; rm -rf ~/.cabal/lib; \
        # echo 'erasing ~/.cabal/packages'; rm -rf ~/.cabal/packages; \
        # echo 'erasing ~/.cabal/share'; rm -rf ~/.cabal/share; \
        )
}

alias cabalupgrades="cabal list --installed  | egrep -iv '(synopsis|homepage|license)'"
