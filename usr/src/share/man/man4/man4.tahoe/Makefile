#	@(#)Makefile	5.4 (Berkeley) 5/11/90

MAN4=	ace.0 autoconf.0 cons.0 cy.0 dr.0 enp.0 ik.0 intro.0 lp.0 mem.0 \
	mtio.0 vd.0 vx.0
MLINKS=	mem.4 kmem.4
MANSUBDIR=/tahoe

all: ${MAN4}

clean depend lint tags:

cleandir:
	rm -f ${MAN4}

install: maninstall

.include <bsd.prog.mk>
