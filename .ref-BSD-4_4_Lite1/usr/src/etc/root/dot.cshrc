set history=1000
set path=(/sbin /usr/sbin /bin /usr/bin /usr/contrib/bin /usr/old/bin)

# directory stuff: cdpath/cd/back
set cdpath=(/sys /usr/src/{bin,sbin,usr.{bin,sbin},lib,libexec,share,contrib,local,games,old})
alias	h	history -r
alias	hup	'kill -HUP `cat /var/run/\!$.pid`'
alias	j	jobs -l
alias	k	jobs -l
alias	ll	ls -l
alias	x	exit
alias	z	suspend

alias	back	'set back=$old; set old=$cwd; cd $back; unset back; dirs'
alias	cd	'set old=$cwd; chdir \!*'
alias	pd	pushd
alias	pd2	pushd +2
alias	pd3	pushd +3
alias	pd4	pushd +4
alias	tset	'set noglob histchars=""; eval `\tset -s \!*`; unset noglob histchars'

if ($?prompt) then
	set prompt="`hostname -s`# "
endif

umask 022
