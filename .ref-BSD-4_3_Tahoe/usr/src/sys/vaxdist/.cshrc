alias mail Mail
set history=100
# directory stuff: cdpath/cd/back
set	cdpath=(/usr/src/{bin,etc,ucb,usr.bin,lib,usr.lib,lib/libc,new} /sys)
alias	cd	'set old=$cwd; chdir \!*'
alias	h	history
alias	j	jobs -l
alias	ls	ls -F
alias	wi	whereis
alias	back	'set back=$old; set old=$cwd; cd $back; unset back; dirs'
# sccs stuff: sd/co/ci/allout/out/unedit
alias	sd	sccs diffs
alias	co	sccs get -e
alias	ci	sccs delget
alias	allout	"(cd ..; echo */SCCS/p.*|sed s/SCCS\\/p.//g)"
alias	out	"echo SCCS/p.*|sed s/SCCS\\/p.//g"
alias	info	sccs info
alias	unedit	sccs unedit
alias	get	sccs get
# spms stuff: chproject/cpd
alias	chproject 'eval `"chproject" -d \!*`'
alias	cpd	'eval `"pd" \!*`'
alias	z		suspend
alias	area	'grep \!* /usr/games/lib/quiz.k/areas'
alias	x	exit
alias	pd	pushd
alias	pd2	pushd +2
alias	pd3	pushd +3
alias	pd4	pushd +4
alias	l	"echo \!*{*}"
set path=(/etc /usr/ucb /bin /usr/bin /usr/local /usr/hosts . /usr/new)
if ($?prompt) then
	set prompt="`hostname | sed 's/\..*//'`# "
endif
