# .cshrc initialization

alias f		finger
alias h		'history -r | more'
alias j		jobs -l
alias la	ls -a
alias lf	ls -FA
alias ll	ls -lgsA
alias tset	'set noglob histchars=""; eval `\tset -s \!*`; unset noglob histchars'
alias x		exit
alias z		suspend

set path=(/usr/ucb /bin /usr/bin /usr/new /usr/local /usr/hosts /usr/games .)

if ($?prompt) then
	# An interactive shell -- set some stuff up
	set ignoreeof
	set mch = `hostname -s`
	set prompt = "$mch:q:$cwd:t {\!} "
	set filec
	set mail = (/usr/spool/mail/$USER)
endif
