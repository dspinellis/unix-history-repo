# shrike.irvine.com - MH 6.7.2 - HP 9000/700 HP/UX 8.x sendmail version
# contributed by Jerry Sweet <jsweet@irvine.COM>
#
# Notes:
#    1. Make sure you're using /bin/make, and not GNU Make.
#
#    2. You may have to modify h/string.h where strlen is declared as follows:
#
#    #if defined(hpux)
#    /* size_t  strlen (); */
#    #else
#    int     strlen ();
#    #endif
#
#    3. vmh.c won't compile.  (Sorry, don't have any patches yet for that.)
#
#
bin	/usr/local/bin/mh-6.7.2
etc	/usr/local/lib/mh-6.7.2
mail	/usr/spool/mail
mandir	/usr/local/man
manuals	standard
chown	/bin/chown
editor	prompter
remove	mv -f
mts	sendmail/smtp
pop	on
options	ATHENA
options	ATZ
options	BIND
options	MHE
options	MHRC
options	MORE='"/usr/bin/more"'
options	MSGPROT='"0600"'
options	NDIR
options	NTOHLSWAP
options	POPUUMBOX
options	SOCKETS
options	SYS5
options	TZNAME
options	TYPESIG=void
options	VSPRINTF
options	WHATNOW
options	DPOP
options	RPOP
options	POP2
options	POPSERVICE='"pop3"'
ccoptions -O
curses	-lcurses -ltermlib
sprintf	int
