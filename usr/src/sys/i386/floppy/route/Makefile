#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	route
SRCS=	route.c
CFLAGS+=-DLETS_GET_SMALL -I.
NOMAN=	noman
LDFLAGS=-N
CLEANFILES+=keywords.h
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../../../../sbin/route

all route depend lint tags: keywords.h

keywords.h: keywords
	sed -e '/^#/d' -e '/^$$/d' \
	    ${.CURDIR}/../../../../sbin/route/keywords > _keywords.tmp
	tr a-z A-Z < _keywords.tmp | paste _keywords.tmp - | \
	    awk '{ \
		if (NF > 1) \
			printf "#define\tK_%s\t%d\n\t{\"%s\", K_%s},\n", \
			    $$2, NR, $$1, $$2 }' \
	    > ${.TARGET}
	rm -f _keywords.tmp

.include <bsd.prog.mk>
