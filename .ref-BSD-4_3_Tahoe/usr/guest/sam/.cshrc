set history=100
if ($?prompt) then
	alias	cd	'set old=$cwd; chdir \!*'
	alias	back	'set back=$old;set old=$cwd;cd $back;unset back;dirs'
	alias	dir	'ls -ls \!* | more'
	alias	h	'history -r | more'
	alias	sd	'sccs get -p \!* | diff - \!$'
	alias	info	sccs info
	alias	co	sccs get -e
	alias	ci	sccs delget
	alias	unget	sccs unedit
	alias	out	"echo SCCS/p.*|sed s/SCCS\\/p.//g"
	alias	allout	"(cd ..; echo */SCCS/p.*|sed s/SCCS\\/p.//g)"
	alias	save	'/usr/new/mh/fakefile +\!*'
	alias	j	jobs -l
	set	prompt="`hostname | sed s/ucb//`% "
endif
