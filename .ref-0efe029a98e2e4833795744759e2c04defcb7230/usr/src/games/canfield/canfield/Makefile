#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	canfield
MAN6=	canfield.0
DPADD=	${LIBCURSES} ${LIBTERM} ${LIBCOMPAT}
LDADD=	-lcurses -ltermcap -lcompat
SUBDIR=	cfscores
HIDEGAME=hidegame

.include <bsd.prog.mk>
