#	@(#)Makefile	4.5 (Berkeley) %G%

all learnlib clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINOWN} -m 444 Linfo Xinfo ${LLIB}/learn
	cp -pr C editor eqn files macros morefiles ${LLIB}/learn

.include "../Makefile.inc"
.include <bsd.own.mk>
