TERM=hp2621-nl
stty dec
PATH=:/usr/local:/usr/ucb:/bin:/usr/bin:/usr/hosts:/usr/games:/usr/local/mh
PS1="`hostname|sed s/ucb//`% "
EXINIT='se ai sm shell=/bin/csh terse'
biff y
msgs -q
PRINTER=ucbvax
export TERM PATH PS1 EXINIT PRINTER
