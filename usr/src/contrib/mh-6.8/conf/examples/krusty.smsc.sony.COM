# krusty.smsc.sony.COM - System V Release 4 running mailsurr
#
# David Elliott 1990, 1991
#
#
bin   /usr/local/bin/mh
etc   /usr/local/lib/mh
mail  /usr/mail
chown /bin/chown
manuals       gen
mts   sendmail/smtp
sprintf       int
ldoptlibs     -lsocket -lnsl -lgen
curses        -lcurses
ranlib        off
options	ATTVIBUG
options	MORE='"/bin/more"'
options	SOCKETS
options	SYS5
options	SYS5DIR
options	SVR4
options	TYPESIG=void
