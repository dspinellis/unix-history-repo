set path=(. /usr/ucb /bin /usr/bin /usr/new /usr/local /usr/hosts)
if ($?prompt) then
    # An interactive shell -- set some stuff up
    set ignoreeof
    set prompt="`hostname | sed s/ucb//`% "
    set history=100
    set savehist=100
endif
