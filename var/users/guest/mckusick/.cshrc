set path=(/bin /sbin /usr/{bin,sbin,games} /usr/{old,contrib,local}/bin ~/bin)
umask 2
if ($?prompt) then
	set time=2
	set mail=(/var/spool/mail/mckusick)
	set prompt="`hostname | sed 's/\..*//'` % "
	set history=200
	set filec
	alias x "time; hashstat; exec date"
	alias X "clear; date; time; hashstat; exec /usr/games/fortune -a"
	setenv EXINIT "set autoindent magic"
	setenv BLOCKSIZE 1k
	setenv MORE -es
	set ignoreeof
	alias h history +20
	alias j jobs -l
	alias f finger
	alias so source
	alias pd pushd
	alias pd2 pushd +2
	alias pd3 pushd +3
	alias pd4 pushd +4
	alias pwd dirs
	alias info sccs info
	alias co sccs edit
	alias ci sccs delget
	alias unget sccs unedit
	alias sp 'sccs prt \!* | more'
	alias sd 'sccs diffs \!* | more'
	alias sc 'sccs get -p \!* | diff -c - \!$ | more'
	alias sx 'sccs get -p \!* | diff -c - \!$ >x'
	alias look '\look -df \!$ /usr/share/dict/web2'
	set cdpath=(/sys /usr/src/{bin,sbin,local,usr.{bin,sbin},contrib,games,lib,libdata,libexec,old,sbin,share} /usr/src ~)
	source ~/.local
endif
