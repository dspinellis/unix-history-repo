DEPENDFILE=	.depend
LIBUTIL=	/usr/lib/libutil.a

.if defined(SUBDIR)

MACHINE != machine
all depend clean cleandir lint tags:
	cd ${SUBDIR}.${MACHINE}; pmake ${.TARGET}

.else

.MAIN: all
all: ${PROGC} ${PROGO}

.if defined(PROGC)
SRCS=	${PROGC}.c

${PROGC}: ${SRCS} ${LIBC} ${SRCLIB}
	${CC} -o ${.TARGET} ${SRCS} ${LDLIB}

clean:
	rm -f core ${PROGC}

depend: ${SRCS}
	mkdep -p ${CFLAGS:M-[ID]*} ${.INCLUDES} ${.ALLSRC}

.endif

.if defined(PROGO)
OBJS=	${SRCS:.c=.o}

${PROGO}: ${OBJS} ${LIBC} ${SRCLIB}
	${CC} -o ${.TARGET} ${OBJS} ${LDLIB}

depend: ${SRCS}
	mkdep ${CFLAGS:M-[ID]*} ${.INCLUDES} ${.ALLSRC}

clean:
	rm -f ${OBJS} core ${PROGO}

${OBJS}: ${.PREFIX}.c

.endif

.if !defined(MAN1)
MAN1=	${MDIR1} ${PROGC}.0
.endif

cleandir: clean
	rm -f ${MANALL} ${TAGSFILE} ${DEPENDFILE}

LINTFLAGS=	-chapbx
lint: ${SRCS}
	lint ${LINTFLAGS} ${CFLAGS} ${.ALLSRC}

TAGSFILE=	tags
tags: ${SRCS}
	ctags ${.ALLSRC}

STRIP=		-s
BINMODE=	755
DEFOWN=		bin
DEFGRP=		bin
MANMODE=	444
MDIR1=		/usr/man/cat1
MDIR2=		/usr/man/cat2
MDIR3=		/usr/man/cat3
MDIR4=		/usr/man/cat4
MDIR5=		/usr/man/cat5
MDIR6=		/usr/man/cat6
MDIR7=		/usr/man/cat7
MDIR8=		/usr/man/cat8
MANALL=	${MAN1} ${MAN2} ${MAN3} ${MAN4} ${MAN5} ${MAN6} ${MAN7} ${MAN8}

install: ${MANALL}
	install ${STRIP} -o ${DEFOWN} -g ${DEFGRP} -m ${BINMODE} \
	    ${PROGC} ${PROGO} ${DESTDIR}${DIR}
.if defined(MAN1)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN1} \
	    ${DESTDIR}${MDIR1}
.if defined(LINKS1)
	rm -f ${DESTDIR}${MDIR1}/${LINKS1}
	for i in ${LINKS1}; do
		ln ${DESTDIR}${MDIR1}/${MAN1} ${DESTDIR}${MDIR1}/$$i
	done
.endif	# LINKS1
.endif	# MAN1
.if defined(MAN2)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN2} \
	    ${DESTDIR}${MDIR2}
.endif	# MAN2
.if defined(MAN3)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN3} \
	    ${DESTDIR}${MDIR3}
.endif	# MAN3
.if defined(MAN4)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN4} \
	    ${DESTDIR}${MDIR4}
.endif	# MAN4
.if defined(MAN5)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN5} \
	    ${DESTDIR}${MDIR5}
.endif	# MAN5
.if defined(MAN6)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN6} \
	    ${DESTDIR}${MDIR6}
.endif	# MAN6
.if defined(MAN7)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN7} \
	    ${DESTDIR}${MDIR7}
.endif	# MAN7
.if defined(MAN8)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN8} \
	    ${DESTDIR}${MDIR8}
.endif	# MAN8
.endif

.if defined(SHAREDSTRINGS)
.NOTPARALLEL:
XSTR=	xstr
.c.o:
	${CC} -E ${.INCLUDES} ${CFLAGS} ${.IMPSRC} | ${XSTR} -c -
	@${CC} ${.INCLUDES} ${CFLAGS} -c x.c -o ${.TARGET}
	@rm -f x.c
.endif
