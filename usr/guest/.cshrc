set history=40
set cdpath=(/usr/sys /usr/src/cmd)
set sys=/usr/sys
alias sd 'sccs get -p \!* | diff - \!$'
alias sgrind 'vgrind `sccs info | sed s/:.\*// | sed s/\ \*// | sort`'
alias ec \
    'echo \!-1:q > /tmp/$$ ; vi /tmp/$$ ; typein `cat /tmp/$$` ; rm /tmp/$$'
alias cd 'set old=$cwd; chdir \!*'
alias back 'set back=$old; set old=$cwd; cd $back; unset back; dirs'
alias dm tail -r /usr/adm/messages
alias uu tail -r /usr/spool/uucp/LOGFILE
alias info sccs info
alias	co		sccs get -e
alias	ci		sccs delget
alias	allout		"(cd $sys; echo */SCCS/p.*|sed s/SCCS\\/p.//g)"
alias	out		"echo SCCS/p.*|sed s/SCCS\\/p.//g"
alias	cot 		set x=\`fgrep ^\!^\	 tags \| \
				awk \'	{ print \$2 \;}\'\` \; \
				co \$x \; vit \!^ \; unset x
alias	md		'make \!* >& make.out &'
alias	netall		'tar cf - \!:2* | net - "mkdir \!^;cd \!^;tar xf -"'
alias	print		netlpr -c vpr
alias	vit		vi -t \!\*
alias c /bin/cat
alias h history
alias vt52 "set term vt52"
alias la36 "set term la36"
