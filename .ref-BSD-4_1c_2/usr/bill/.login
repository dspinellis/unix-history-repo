set path=(/usr/new /usr/ucb /bin /usr/bin /usr/hosts /usr/local /usr/new/mh .)
setenv EXINIT 'se ai shell=/bin/csh terse nowarn sm'
set ignoreeof time=15
alias ts \
 'unsetenv TERM TERMCAP;  set noglob ; eval `tset -s -m dialup:c1004p -m plugboard:?hp2621nl \!*`';
alias . logout
stty dec
if ($TERM == c100) setenv TERM concept		# for stupid emacs
uptime
alias . exec /bin/date
setenv MORE c
