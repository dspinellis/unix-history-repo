#	@(#)Makefile	1.2 (Berkeley) %G%
# transcript/man/Makefile
#
# Copyright 1985 Adobe Systems Incorporated
#
# RCSID: $Header: Makefile,v 2.1 85/11/24 12:38:23 shore Rel $
# RCSLOG:
# $Log:	Makefile,v $
# Revision 2.1  85/11/24  12:38:23  shore
# Product Release 2.0
# 
# 
#

.SUFFIXES: .1 .1p .7 .7p .8 .8p

PAGES1 = enscript.1 ps4014.1 ps630.1 pscat.1 psdit.1 psplot.1 psrev.1 \
	psroff.1 ptroff.1 loadseal.1
PAGES7 = afm.7 postscript.7
PAGES8 = pscatmap.8 transcript.8

PAGES = ${PAGES1} ${PAGES7} ${PAGES8}

programs all: ${PAGES}

.1p.1 .7p.7 .8p.8:
	sed	-e s,XPSLIBDIRX,$$PSLIBDIR,g \
		-e s,XTROFFFONTDIRX,$$TROFFFONTDIR,g \
		-e s,XPSTEMPDIR,$$PSTEMPDIR,g \
		-e s,XDITDIRX,$$DITDIR,g \
		$? > $@

install: ${PAGES}
	cd ${MANDIR}; rm -f ${PAGES}
	./installman 1 $$MAN1
	./installman 7 $$MAN7
	./installman 8 $$MAN8

clean:
	rm -f - ${PAGES} *.BAK *.CKP .emacs_[0-9]*
