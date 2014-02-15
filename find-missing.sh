find .cask -name '*.el' | sed 's/$/c/' | grep -v -- '-autoloads.elc$' | xargs stat >/dev/null
