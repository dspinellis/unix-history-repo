#	@(#)Makefile	4.11 (Berkeley) %G%

CFLAGS=	-I${.CURDIR}/../common_source
LIBC=	/usr/lib/libc.a
DEST=	/usr/libdata/term
PROGS=	tab300 tab300-12 tab302 tab302-12 tab37 tab382 tab382-12 tab450 \
	tab450-12 tab833 tab833-12 tabepson tabitoh tabitoh12 tablpr \
	tabnec tabnec-t tabnec12 tabnec25-t tabqume tabqume12 tabx-ecs \
	tabx-ecs12 tabxerox tabxerox12
OBJS=	${PROGS:=.o}
SRCS=	${PROGS:=.c}
LINKS=	${DEST}/tablpr ${DEST}/tabtn300 ${DEST}/tablpr ${DEST}/tabcrt \
	${DEST}/tab302 ${DEST}/tab300s ${DEST}/tab300s-12 ${DEST}/tab302-12 \
	${DEST}/tabdtc ${DEST}/tab302 ${DEST}/tabdtc12 ${DEST}/tab302-12 \
	${DEST}/tabipsi ${DEST}/tab450 ${DEST}/tabipsi12 ${DEST}/tab450-12

all: ${PROGS}

${PROGS}: mktab.o ${.CURDIR}/${.PREFIX.c}
	${CC} mktab.o ${.CURDIR}/${.PREFIX}.c -o mktab
	./mktab > $@

clean:
	rm -f ${OBJS} ${PROGS} mktab.o mktab a.out Errs errs mklog core

cleandir: clean
	rm -f ${.CURDIR}/tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS:M-[ID]*} ${.ALLSRC}

beforeinstall:
	install -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} ${PROGS} \
	    ${DESTDIR}${DEST}
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/README \
	    ${DESTDIR}${DEST}

lint: ${SRCS}
	${LINT} ${LINTFLAGS} ${CFLAGS} ${.ALLSRC}

tags: ${SRCS}
	${CTAGS} -f ${.CURDIR}/tags ${.ALLSRC}

.include <bsd.prog.mk>
