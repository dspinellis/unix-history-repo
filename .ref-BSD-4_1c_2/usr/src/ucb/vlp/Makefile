#$Header: /usr/src/ucb/vlp/RCS/Makefile,v 1.2 83/02/10 13:26:04 jkf Exp $
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
	install vlp ${DESTDIR}/usr/ucb
	install -c -m 664 vlpmacs ${DESTDIR}/usr/lib
	install -c -m 775 ilprint.sh ${DESTDIR}/usr/ucb/ilprint
	install -c -m 775 vlprint.sh ${DESTDIR}/usr/ucb/vlprint

clean:
	rm -f vlp
