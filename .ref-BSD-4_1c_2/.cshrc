alias mail Mail
set history=40
# directory stuff: cdpath/cd/back
set cdpath=(/a/wnj)
alias	cd	'set old=$cwd; chdir \!*'
alias	back	'set back=$old; set old=$cwd; cd $back; unset back; dirs'
# sccs stuff: sd/co/ci/allout/out/unedit
alias	sd	'sccs get -p \!* | diff - \!$'
alias	co	sccs get -e
alias	ci	sccs delget
alias	allout	"(cd ..; echo */SCCS/p.*|sed s/SCCS\\/p.//g)"
alias	out	"echo SCCS/p.*|sed s/SCCS\\/p.//g"
alias	info	sccs info
alias	unedit	sccs unedit
alias	z		suspend
alias	area	'grep \!* /usr/games/lib/quiz.k/areas'
alias	tel	'grep -i \!* /usr/bill/bin/telno'
alias	x	exit
alias	pd	pushd
alias	pd2	pushd +2
alias	pd3	pushd +3
alias	pd4	pushd +4
alias	j	jobs -l
set path=(/etc /usr/ucb /bin /usr/bin /usr/local /usr/hosts .)
if ($?prompt) then
	set prompt="`hostname | sed s/ucb//`# "
endif
