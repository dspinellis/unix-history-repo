alias ts 'set noglob ; eval `tset -s \!*`';
set onether = `tty | sed s,/dev/tty,, | sed s,.\$,,`
if ($onether != p && $onether != q) unset onether
if (! $?onether) then
	ts -m 'dialup:?c108-8p' '?5620'
else if ($TERM == "5620" && -e $HOME/.jermcap) then
	rs
else
	ts $TERM
endif
if ($TERM == h19) then
	setenv TERM h19-u
endif
stty crt intr ^C kill ^U erase ^H
setenv ROGUEOPTS "jump,terse,name=spiderman,flush,fruit=peanut,askme"
if (`tty` == '/dev/console') then
	echo "logged in on console, turning off tabs."
	stty -tabs
endif
if (! $?onether) then
	echo -n "Run sysline (Yes) ?"
	set ans=($<)
	if (($ans == 'n') || ($ans == 'no')) then
		unset ans
	else
		dirs >! ~/.who
		set ans = `sysline -q -i -r -h +60`
		if ("$ans" == "") unset ans
	endif
endif
if ($?ans) then
	setenv SYSLINE "$ans"
	alias newsysline "kill -ALRM $ans"
	alias killsysline "kill -HUP $ans"
	alias clearsysline "killsysline; unalias newsysline killsysline clearsysline cd popd dirs; source ~/.cshrc"
	unset ans
	alias x "killsysline; `alias x`"
	alias X "killsysline; `alias X`"
	alias 'pd' "pushd \!* >! ~/.who ; newsysline"
	alias 'pd2' "pushd +2 \!* >! ~/.who ; newsysline"
	alias 'pd3' "pushd +3 \!* >! ~/.who ; newsysline"
	alias 'pd4' "pushd +4 \!* >! ~/.who ; newsysline"
	alias 'popd' "popd \!* >! ~/.who ; newsysline"
	alias 'dirs' "dirs >! ~/.who ; newsysline"
	alias 'cd' "cd \!* ; dirs"
endif
alias mail Mail
set mail=(/usr/spool/mail/mckusick)
