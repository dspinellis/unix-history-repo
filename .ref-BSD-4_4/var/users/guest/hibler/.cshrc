if ($?prompt) then
	set prompt = "\! `hostname | sed s/\.CS\.Berkeley\.EDU//`> "
	set ignoreeof
	set notify
	if ( -e /var/mail ) then
		set mail = /var/mail/$USER
	else
		set mail = /usr/mail/$LOGNAME
	endif
	set history = 100
	alias pwd 'echo "$cwd"'
	stty erase ^H
	source ~/.aliases
endif
