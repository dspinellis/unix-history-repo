#	@(#)Makefile	5.5 (Berkeley) %G%
#
# $Header: Makefile,v 1.3 85/03/26 15:03:26 lwall Exp $

BINDIR = $(DESTDIR)/usr/new
CFLAGS = -O

all: patch patch.0

patch: patch.c
	${CC} patch.c $(CFLAGS) -o patch

patch.0: patch.n
	../../man/manroff patch.n > patch.0

install:
	install -s -o bin -g src -m 755 patch $(BINDIR)
	install -c -o bin -g src -m 444 patch.0 ${DESTDIR}/usr/new/man/cat1/patch.0

clean:
	rm -f patch patch.kit a.out core errs

shar:
	shar patch.man patch.c Makefile >patch.kit

lint:
	lint -phbvxac patch.c
