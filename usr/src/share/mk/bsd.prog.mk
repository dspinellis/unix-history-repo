# user defines:
#	SRCS		list of source files
#	PROGC		program consisting of a single .c module
#	PROGO		program consisting of multiple .c modules
#	SRCLIBS		list of libraries program depends on
#	LDLIBS		list of libraries for loader

all: ${PROGC} ${PROGO}

.if defined(PROGC)
SRCS=	${PROGC}.c

.if !defined(MAN1)
MAN1=	${PROGC}.0
.endif

${PROGC}: ${SRCS} ${LIBC} ${SRCLIBS}
	${CC} -o ${.TARGET} ${.ALLSRC} ${LDLIBS}

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${.ALLSRC}

clean:
	rm -f core ${PROGC}
.else
OBJS=	${SRCS:.c=.o}

.if !defined(MAN1)
MAN1=	${PROGO}.0
.endif

${PROGO}: ${OBJS} ${LIBC} ${SRCLIBS}
	${CC} -o ${.TARGET} ${OBJS} ${LDLIBS}

{OBJS}: ${.PREFIX}.c

depend: ${SRCS}
	mkdep ${CFLAGS} ${.ALLSRC}

clean:
	rm -f ${OBJS} core ${PROGO}
.endif

# user defines:
#	MAN1		list of man targets for section 1
#	...
#	MAN8		list of man targets for section 8
MANALL=	${MAN1} ${MAN2} ${MAN3} ${MAN4} ${MAN5} ${MAN6} ${MAN7} ${MAN8}

cleandir: clean
	rm -f ${MANALL} ${TAGSFILE} ${DEPENDFILE}

STRIP=		-s
BINMODE=	755
DEFOWN=		bin
DEFGRP=		bin
MANMODE=	444

install: ${MANALL}
	install ${STRIP} -o ${DEFOWN} -g ${DEFGRP} -m ${BINMODE} \
	    ${PROGC} ${PROGO} ${DESTDIR}/${DIR}
.if defined(MAN1)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN1} \
	    ${DESTDIR}/usr/cat1
.endif
.if defined(MAN2)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN2} \
	    ${DESTDIR}/usr/cat2
.endif
.if defined(MAN3)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN3} \
	    ${DESTDIR}/usr/cat3
.endif
.if defined(MAN4)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN4} \
	    ${DESTDIR}/usr/cat4
.endif
.if defined(MAN5)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN5} \
	    ${DESTDIR}/usr/cat5
.endif
.if defined(MAN6)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN6} \
	    ${DESTDIR}/usr/cat6
.endif
.if defined(MAN7)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN7} \
	    ${DESTDIR}/usr/cat7
.endif
.if defined(MAN8)
	install -c -o ${DEFOWN} -g ${DEFGRP} -m ${MANMODE} ${MAN8} \
	    ${DESTDIR}/usr/cat8
.endif

LINTFLAGS=	-chapbx
lint: ${SRCS}
	lint ${LINTFLAGS} ${CFLAGS} ${.ALLSRC}

TAGSFILE=	tags
tags: ${SRCS}
	ctags ${.ALLSRC}

.include <template.mk>

.if exists(.depend)
.include ".depend"
.endif
