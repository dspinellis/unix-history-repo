#	@(#)Makefile	4.13 (Berkeley) 6/24/90

CFLAGS+=-I${.CURDIR}/../common_source
DEST=	/usr/libdata/term
PROGS=	tab300 tab300-12 tab302 tab302-12 tab37 tab382 tab382-12 tab450 \
	tab450-12 tab833 tab833-12 tabepson tabitoh tabitoh12 tablpr \
	tabnec tabnec-t tabnec12 tabnec25-t tabqume tabqume12 tabx-ecs \
	tabx-ecs12 tabxerox tabxerox12
OBJS=	${PROGS:=.o} mktab.o
SRCS=	${PROGS:=.c} mktab.c
LINKS=	${DEST}/tablpr ${DEST}/tabtn300 ${DEST}/tablpr ${DEST}/tabcrt \
	${DEST}/tab302 ${DEST}/tab300s ${DEST}/tab302-12 ${DEST}/tab300s-12 \
	${DEST}/tab302 ${DEST}/tabdtc ${DEST}/tab302-12 ${DEST}/tabdtc12 \
	${DEST}/tab450 ${DEST}/tabipsi ${DEST}/tab450-12 ${DEST}/tabipsi12
BINMODE=444
NOMAN=	noman

all: ${PROGS}

${PROGS}: mktab.o ${.PREFIX}.o
	${CC} mktab.o ${.PREFIX}.o -o mktab
	./mktab > ${.TARGET}

clean::
	rm -f ${OBJS} ${PROGS} mktab a.out Errs errs mklog core

cleandir:: clean
	rm -f ${.CURDIR}/tags .depend

depend: .depend
.depend: ${SRCS} mktab.c
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
