#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	troff
CFLAGS+=-DVMUNIX -I${.CURDIR}/../common_source
SRCS=	ni.c nii.c n1.c n2.c n3.c n4.c n5.c t6.c n7.c n8.c n9.c t10.c \
	hytab.c suftab.c tab3.c
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
.PATH:	${.CURDIR}/../common_source

hytab.o suftab.o:
	${CC} ${CFLAGS} -R -c ${.IMPSRC}

.include <bsd.prog.mk>
