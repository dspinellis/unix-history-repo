set history=80 savehist=80
# directory stuff: cdpath/cd/back
set cdpath=(/usr/sys /usr/src/cmd)
alias	cd	'set old=$cwd; chdir \!*'
alias	back	'set back=$old; set old=$cwd; cd $back; unset back; dirs'
# sccs stuff: sd/co/ci/allout/out/unedit
alias	sd	'sccs get -p \!* | diff - \!$'
alias	co	sccs get -e
alias	ci	sccs delget
alias	cci	'(echo "0a"; echo "/* %M% %I% %E% */"; echo ""; echo "."; echo "w"; echo "q")|ed - \!$;sccs admin -i\!$ \!$; mv \!$ /tmp; co \!$'
alias	allout	"(cd ..; echo */SCCS/p.*|sed s/SCCS\\/p.//g)"
alias	out	"echo SCCS/p.*|sed s/SCCS\\/p.//g"
alias	info	sccs info
alias	unedit	sccs unedit
# system status stuff: dm/uu
alias	dm	tail -r /usr/adm/messages
alias	uu	tail -r /usr/spool/uucp/LOGFILE
alias	ec \
	'echo \!-1:q > /tmp/$$;vi /tmp/$$;typein `cat /tmp/$$`;rm /tmp/$$'
alias	z		suspend
alias file	fakefile
alias	g	'vgrind -h "4.1bBSD\ July\ 1,\ 1982" \!*; echo pickup \!* | mail wnj'
alias	gn	'vgrind -n -h "4.1bBSD July 1, 1982" \!*; echo pickup \!* | mail wnj'
alias	area	'grep \!* /usr/games/lib/quiz.k/areas'
alias	tel	'grep -i \!* ~/bin/telno'
alias	pl	tail -r /usr/spool/berknet/plogfilev
alias ecc 'cd ~ecc'
if ($?prompt) then
	set prompt="`hostname | sed s/ucb//` % "
endif
alias fold ls /usr/bill/mail
alias msgs readnews
