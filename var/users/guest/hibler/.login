# start up .login
tset -Q -m'network:?xterms'
stty newcrt status  kill  erase 
set path=(~/bin /sbin /usr/sbin /bin /usr/bin /usr/local/bin /usr/contrib/bin /usr/old/bin .)
biff y
setenv TERMCAP /usr/share/misc/termcap
setenv BLOCKSIZE 1k
umask 22
msgs -q
