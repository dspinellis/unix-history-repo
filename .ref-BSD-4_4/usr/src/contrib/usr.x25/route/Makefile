#	@(#)Makefile	5.5 (Berkeley) 11/29/90

PROG=	route
MAN8=	route.0
CFLAGS+=-I.
LDADD=-lx25
CLEANFILES+=keywords.h
BINOWN=	root
BINMODE=4555

all route depend lint tags: keywords.h

keywords.h: keywords
	sed -e '/^#/d' -e '/^$$/d' ${.CURDIR}/keywords > _keywords.tmp
	tr a-z A-Z < _keywords.tmp | paste _keywords.tmp - | \
	    awk '{ \
		if (NF > 1) \
			printf "#define\tK_%s\t%d\n\t{\"%s\", K_%s},\n", \
			    $$2, NR, $$1, $$2 }' \
	    > ${.TARGET}
	rm -f _keywords.tmp

.include <bsd.prog.mk>
