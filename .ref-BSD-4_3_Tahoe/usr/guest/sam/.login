alias	ts	'set noglob ; eval `tset -Q -s -m dialup:c1004p \!*`; unset noglob';
stty dec -nl nl0
set ignoreeof time=15
set path=(. /usr/local /usr/ucb /bin /usr/bin /usr/hosts /usr/games /usr/local/mh)
set prompt="`hostname|sed s/ucb//`% "
setenv EXINIT 'se ai sm shell=/bin/csh terse'
alias	.	logout
biff y
msgs -q
setenv PRINTER ucbvax
