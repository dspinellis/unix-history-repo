set path=(/usr/new /usr/ucb /bin /usr/bin /usr/hosts /usr/local /usr/public /usr/games .)
set time=2
umask 2
if ($?prompt) then
	set prompt="`hostname | sed s/ucb//` % "
	set history=200
	set savehist=200
	alias x "time; hashstat; exec date"
	alias X "clear; date; time; hashstat; exec /usr/games/fortune -a"
	setenv EXINIT "set autoindent magic"
	set ignoreeof
	alias up 'ruptime | egrep "ucbvax|ernie|arpa|calder|monet"'
	alias h history +20
	alias j jobs -l
	alias so source
	alias pd pushd
	alias pd2 pushd +2
	alias pd3 pushd +3
	alias pd4 pushd +4
	alias pwd dirs
	alias rw "rwho -a | egrep \!*"
	source ~/.local
endif
