alias /		suspend
alias M		Mail
alias N		vnews
alias area	"egrep \!* /usr/games/lib/quiz.k/areas"
alias b		echo 
alias call	uucico -r1 -s"\!*" -x9
alias cont	kill -CONT
alias cst	tr -d "'\001-\010\013-\037\177-\377'"
alias ec	'echo \!-1:q > /tmp/_$$;ev /tmp/_$$;eval `cat /tmp/_$$`'
alias fl	lint -chapbx "\!*"
alias from	'from | tail'
alias fuck	"echo \!* > /dev/null;echo sorry... I\'m doing my best."
alias h		history -r
alias k		jobs -l
alias la	ls -a
alias lf	ls -FA
alias ll	ls -lgsA
alias lock	ls /usr/spool/uucp/LCK
alias lu	ls "\!*" /usr/spool/uucp
alias make	"make \!*;echo "
alias rs	'set noglob;eval `resize`;unset noglob'
alias sl	ls
alias sp	look "\!*" /usr/dict/words
alias sline	sysline -mhD
alias t		vi -t "\!*"
alias ta	tail "\!*" /usr/spool/mqueue/syslog
alias ts	tail "\!*" /usr/adm/syslog
alias tu	tail "/usr/spool/uucp/LOG/uucico/\!*"
alias dx	'uucico -r1 -x4 -s\!* >& /tmp/_\!*'
alias ux	'uucico -r1 -x1 -s\!* >& /tmp/_\!*'
alias wh	look "\!*" /usr/local/lib/pathalias/paths

# sccs stuff
alias allout	"(cd ..; echo */SCCS/p.*|sed s/SCCS\\/p.//g)"
alias csd	"sccs diffs \!* | more"
alias dcsd	'sccs get -p \!* | diff -c - \!$ | more'
alias sci	"sccs delget"
alias sco	"sccs get -e"
alias sd	"sccs sccsdiff \!* | more"
alias get	"sccs get"
alias info	"sccs info"
alias out	"echo SCCS/p.*|sed s/SCCS\\/p.//g"
alias prt	"sccs prt \!* | more"
alias unedit	"sccs unedit"

set cdpath = (/usr/lib /usr/spool/news/net /usr/src/{man,bin,etc,games,include,lib,local,new,old,sys,ucb,usr.bin,usr.lib} /usr/src ~bostic)
set filec
set histchars = '\!;'
set history = 1000
set mail = /usr/spool/mail/bostic
set path = (~bostic/bin /usr/ucb /bin /usr/bin /usr/local /etc /usr/lib . /usr/games /usr/hosts /usr/lib/uucp /usr/new)
set shell = /bin/sh
set t = /tmp/_f
set time = 5
set u = /usr/spool/uucp
set ud = /usr/spool/uucp/D.seismo

setenv DTF		~bostic/bin
setenv EDITOR		~bostic/bin/ev
setenv HOME		~bostic
setenv HOSTALIASES	~bostic/.hostaliases
setenv MAIL		/usr/spool/mail/bostic
setenv MORE		-cs
setenv PRINTER		lz

set mch = `hostname | sed -e 's/\..*//'`
if ( `whoami` == "root")then
	alias cd 'cd \!:*; set prompt = "$mch:q:$cwd:t [\\!] "'
	set prompt = "$mch:q:$cwd:t [\!] "
	umask 2
else
	alias cd 'cd \!:*; set prompt = "$mch:q:$cwd:t {\\!} "'
	set prompt = "$mch:q:$cwd:t {\!} "
	umask 2
endif
