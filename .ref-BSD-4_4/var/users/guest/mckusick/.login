alias ts 'set noglob ; eval `tset -s \!*`';
set onether = `tty | sed s,/dev/tty,, | sed s,.\$,,`
if ($onether != p && $onether != q) unset onether
if (! $?onether) then
	ts -m 'dialup:?c108-8p' '?xterm'
else
	ts $TERM
endif
stty crt intr ^C kill ^U erase ^H
alias mail Mail
