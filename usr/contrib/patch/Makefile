#	@(#)Makefile	5.4 (Berkeley) 5/31/86
#
# $Header: Makefile,v 1.3 85/03/26 15:03:26 lwall Exp $

BINDIR = $(DESTDIR)/usr/new
CFLAGS = -O

patch: patch.c
	cc patch.c $(CFLAGS) -o patch

install: patch
	install -s patch $(BINDIR)
	install -c -m 444 patch.n ${DESTDIR}/usr/man/mann/patch.n

clean:
	rm -f patch patch.kit a.out core errs

shar:
	shar patch.man patch.c Makefile >patch.kit

lint:
	lint -phbvxac patch.c
