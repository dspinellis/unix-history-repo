#	@(#)Makefile	4.12 (Berkeley) %G%

PROG=	adventure
MAN6=	adventure.0
SRCS=	done.c init.c io.c main.c save.c subr.c vocab.c wizard.c
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
.PATH:	${.CURDIR}
HIDEGAME=hidegame

save.o: save.c			# vax C optimizer bug - infinite loop
	${CC} -c ${.CURDIR}/save.c

beforeinstall:
	cp ${.CURDIR}/glorkz .
	mv adventure adventure.out
	./adventure.out
	rm -f adventure.out

.include <bsd.prog.mk>
