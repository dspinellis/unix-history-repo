#	@(#)Makefile	5.1 (Berkeley) %G%
#
# $Header: Makefile,v 1.3 85/03/26 15:03:26 lwall Exp $

BINDIR = $(DESTDIR)/usr/new

patch: patch.c
	cc patch.c -O -o patch

install:
	install -c patch $(BINDIR)
	install -c -m 444 patch.n ${DESTDIR}/usr/man/mann/patch.n

clean:
	rm patch

shar:
	shar patch.man patch.c Makefile >patch.kit

lint:
	lint -phbvxac patch.c
