#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	mtree
SRCS=	compare.c cwalk.c mtree.c spec.c util.c
.PATH:	${.CURDIR}

mtree.0: mtree.1
	tbl ${.CURDIR}/mtree.1 | nroff -h -man > $@

.include <bsd.prog.mk>
