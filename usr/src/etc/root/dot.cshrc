set history=1000
set path=(/sbin /usr/sbin /bin /usr/bin)

# directory stuff: cdpath/cd/back
set cdpath=(/sys /usr/src/{bin,sbin,usr.{bin,sbin},lib,libexec,share,contrib,local,games,old})
alias	cd	'set old=$cwd; chdir \!*'
alias	h	history -r
alias	hup	'kill -HUP `cat /var/run/\!$.pid`'
alias	j	jobs -l
alias	ll	ls -lg
alias	ls	ls -g
alias	back	'set back=$old; set old=$cwd; cd $back; unset back; dirs'

alias	z	suspend
alias	x	exit
alias	pd	pushd
alias	pd2	pushd +2
alias	pd3	pushd +3
alias	pd4	pushd +4
alias	tset	'set noglob histchars=""; eval `\tset -s \!*`; unset noglob histchars'

if ($?prompt) then
	set prompt="`hostname -s`# "
endif

umask 022
