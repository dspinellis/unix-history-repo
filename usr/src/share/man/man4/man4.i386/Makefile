#	@(#)Makefile	8.1 (Berkeley) %G%

MAN4=	mem.0
MLINKS=	mem.4 kmem.4
MANSUBDIR=/i386

all: ${MAN4}

clean depend lint tags:

cleandir:
	rm -f ${MAN4}

install: maninstall

.include <bsd.prog.mk>
