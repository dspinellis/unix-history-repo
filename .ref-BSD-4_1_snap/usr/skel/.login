set path=(/usr/new /usr/ucb /bin /usr/bin /usr/local /usr/games .)
setenv EXINIT 'se ai shell=/bin/csh terse nowarn sm'
set ignoreeof time=15
alias ts \
 'set noglob ; eval `tset -s -m dialup:c100rv4pna -m plugboard:?hp2621nl \!*`';
alias . logout
ts; stty intr ^C kill ^U; biff y
uptime
