
PROG=	init
#SRCS=	init.c 
DPADD=	${LIBUTIL}
LDADD=	-lutil
NOMAN=noman
LDFLAGS=

beforeinstall:
	rm -f ${DESTDIR}/sbin/init.old
	ln ${DESTDIR}/sbin/init ${DESTDIR}/sbin/init.old

.include <bsd.prog.mk>
