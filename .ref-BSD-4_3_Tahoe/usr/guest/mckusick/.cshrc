set path=(/usr/new /usr/ucb /bin /usr/bin /usr/hosts /usr/local /usr/new/mh /usr/public /usr/games .)
set time=2
umask 2
if ($?prompt) then
	set prompt="`hostname | sed 's/\..*//'` % "
	set history=200
	set savehist=200
	set filec
	alias x "time; hashstat; exec date"
	alias X "clear; date; time; hashstat; exec /usr/games/fortune -a"
	setenv EXINIT "set autoindent magic"
	set ignoreeof
	alias up 'ruptime | egrep "calder|ucbvax|matisse|arpa|vangogh|monet"'
	alias h history +20
	alias j jobs -l
	alias so source
	alias pd pushd
	alias pd2 pushd +2
	alias pd3 pushd +3
	alias pd4 pushd +4
	alias pwd dirs
	alias rw "rwho -a | egrep \!*"
	alias info sccs info
	alias co sccs edit
	alias ci sccs delget
	alias unget sccs unedit
	alias sp 'sccs prt \!* | more'
	alias sd 'sccs diffs \!* | more'
	alias look '\look -df \!$ /usr/dict/web2'
	set cdpath=(/sys /usr/src/{bin,etc,ucb,usr.bin,lib,usr.lib,lib/libc,new} /usr/src ~)
	source ~/.local
endif
