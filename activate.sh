# activate.sh
# Sets up the shell environment.

# Get absolute path of this directory. Below snippet from
# stackoverflow.com/questions/59895:
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
  THIS_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$THIS_DIR/$SOURCE" 
done

export DOTFILES="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

if [[ ":$PATH:" != *":$DOTFILES/bin:"* ]]; then
    export PATH="$PATH:$DOTFILES/bin"
fi

# Anything ending in .dot.xx.sh gets sourced in numerical order, then anything
# ending in .dot.sh. 
if [[ $(uname -a) == *Darwin* ]]; then
    find_re="find -E $DOTFILES -regex"
else
    find_re="find $DOTFILES -regextype posix-extended -regex"
fi

for f in $($find_re ".+\.dot\.[0-9][0-9]\.sh" | sort -t "." -k 2); do
    source $f
done

for f in $(find $DOTFILES -name "*.dot.sh"); do
    source "$f"
done
