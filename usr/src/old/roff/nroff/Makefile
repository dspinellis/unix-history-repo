#	@(#)Makefile	4.7 (Berkeley) 5/11/90

PROG=	nroff
CFLAGS+=-DNROFF -DVMUNIX -I${.CURDIR}/../common_source
SRCS=	ni.c nii.c n1.c n2.c n3.c n4.c n5.c n6.c n7.c n8.c n9.c n10.c \
	hytab.c ntab.c suftab.c
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
.PATH:	${.CURDIR}/../common_source

hytab.o suftab.o ntab.o:
	${CC} ${CFLAGS} -R -c ${.IMPSRC}

.include <bsd.prog.mk>
