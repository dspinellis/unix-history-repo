set path=(/usr/ucb /bin /usr/bin /usr/local /usr/games .)
setenv EXINIT 'se ai shell=/bin/csh terse nowarn sm'
set ignoreeof time=15
alias ts \
 'set noglob; eval `tset -s vt100`'
alias . logout
ts; stty dec crt; biff y
uptime
alias . exec /bin/date
