#	@(#)Makefile	5.1 (Berkeley) %G%

PROGS=	fortune strfile unstr
SRCS=	fortune.c strfile.c unstr.c
MAN6=	fortune.0
BINOWN=	games
DATFILES=fortunes.dat fortunes-o.dat startrek.dat zippy.dat
.PATH:	${.CURDIR} ${.CURDIR}/datfiles

all: ${PROGS} ${MAN6} ${DATFILES}

${PROGS}: ${LIBC} ${.PREFIX}.c
	${CC} ${CFLAGS} -o ${.TARGET} ${.CURDIR}/${.PREFIX}.c

${DATFILES}: ${.TARGET:R}
	./strfile -rs ${.CURDIR}/datfiles/${.TARGET:R} ${.TARGET}

clean:
	rm -f ${PROGS} core *.dat

cleandir: clean
	rm -f ${MAN6} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${.ALLSRC}

install:
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} fortune \
	    ${DESTDIR}/usr/games
	install -c -o ${MANOWN} -g ${MANGRP} -m ${MANMODE} fortune.0 \
	    ${DESTDIR}/usr/share/man/cat6
	(cd ${.CURDIR}/datfiles; install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${DATFILES:R} ${DESTDIR}/usr/share/games/fortune)
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${DATFILES} \
	    ${DESTDIR}/usr/share/games/fortune

lint: ${SRCS}
	cd ${.CURDIR}; lint ${CFLAGS} ${LINTFLAGS} fortune.c
	cd ${.CURDIR}; lint ${CFLAGS} ${LINTFLAGS} strfile.c
	cd ${.CURDIR}; lint ${CFLAGS} ${LINTFLAGS} unstr.c

tags: ${SRCS}
	cd ${.CURDIR}; ctags fortune.c
	cd ${.CURDIR}; ctags strfile.c
	cd ${.CURDIR}; ctags unstr.c
	cd ${.CURDIR}; sort -o tags tags

.include <bsd.prog.mk>
