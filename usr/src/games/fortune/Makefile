#	@(#)Makefile	5.2 (Berkeley) 6/26/90

PROGS=	fortune strfile unstr
SRCS=	fortune.c strfile.c unstr.c
MAN6=	fortune.0
BINOWN=	games
DATFILES=fortunes.dat startrek.dat zippy.dat fortunes-o.dat
.PATH:	${.CURDIR} ${.CURDIR}/datfiles
CLEANFILES+=strfile unstr

all: ${PROGS} ${MAN6} ${DATFILES}

${PROGS}: ${LIBC} ${.PREFIX}.c
	${CC} ${CFLAGS} -o ${.TARGET} ${.CURDIR}/${.PREFIX}.c

fortunes-o.dat: ${.TARGET:R}
	./strfile -rsx ${.CURDIR}/datfiles/${.TARGET:R} ${.TARGET}
fortunes.dat startrek.dat zippy.dat: ${.TARGET:R}
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
