#$Header: /usr/src/usr.ucb/vlp/vlp.vax/RCS/Makefile,v 1.7 87/05/31 21:17:58 bostic Exp Locker: bostic $
#
#			-[Thu Feb 10 13:15:17 1983 by jkf]-

# Makefile for VLP

# files required:
#	vlp.l	- lisp program to read source and add troff control
#		characters.
#	vlpmacs - troff macros used by vlp.
#
# vlp and vlpmacs are put in DESTDIR
# If DESTDIR is changed, then the source (vlp.l) must be modified since
# it explicitly points to vlpmacs.

DESTDIR =
.DEFAULT:vlp

vlp:	vlp.l
	/usr/ucb/liszt -qar -o vlp vlp.l

install: vlp vlpmacs
	install -o bin -g bin -m 755 vlp ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 664 vlpmacs ${DESTDIR}/usr/libdata

clean:
	rm -f vlp

depend:
