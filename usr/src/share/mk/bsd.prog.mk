.include <bsd.global.mk>

.if exists(${.CURDIR}/../Makefile.inc)
.include "${.CURDIR}/../Makefile.inc"
.endif

LIBC?=		/lib/libc.a
LIBCOMPAT?=	/usr/lib/libcompat.a
LIBCURSES?=	/usr/lib/libcurses.a
LIBDBM?=	/usr/lib/libdbm.a
LIBDES?=	/usr/lib/libdes.a
LIBL?=		/usr/lib/libl.a
LIBKDB?=	/usr/lib/libkdb.a
LIBKRB?=	/usr/lib/libkrb.a
LIBM?=		/usr/lib/libm.a
LIBMP?=		/usr/lib/libmp.a
LIBPC?=		/usr/lib/libpc.a
LIBPLOT?=	/usr/lib/libplot.a
LIBRPC?=	/usr/lib/sunrpc.a
LIBTERM?=	/usr/lib/libterm.a
LIBUTIL?=	/usr/lib/libutil.a

.if defined(SRCS)

OBJS+=	${SRCS:.c=.o}

${PROG}: ${OBJS} ${LIBC} ${DPADD}
	${CC} ${LDFLAGS} -o ${.TARGET} ${OBJS} ${LDADD}

.else

SRCS= ${PROG}.c

${PROG}: ${SRCS} ${LIBC} ${DPADD}
	${CC} ${CFLAGS} -o ${.TARGET} ${.CURDIR}/${SRCS} ${LDADD}

.endif

.if	!defined(MAN1) && !defined(MAN2) && !defined(MAN3) && \
	!defined(MAN4) && !defined(MAN5) && !defined(MAN6) && \
	!defined(MAN7) && !defined(MAN8) && !defined(NOMAN)
MAN1=	${PROG}.0
.endif
MANALL=	${MAN1} ${MAN2} ${MAN3} ${MAN4} ${MAN5} ${MAN6} ${MAN7} ${MAN8}

PROGSUBDIR: .USE
.if defined(SUBDIR) && !empty(SUBDIR)
	@for entry in ${SUBDIR}; do \
		(echo "===> $$entry"; \
		if test -d ${.CURDIR}/$${entry}.${MACHINE}; then \
			cd ${.CURDIR}/$${entry}.${MACHINE}; \
		else \
			cd ${.CURDIR}/$${entry}; \
		fi; \
		${MAKE} ${.TARGET}) \
	done
.endif

.MAIN: all
all: ${PROG} ${MANALL} PROGSUBDIR

.if !target(clean)
clean: PROGSUBDIR
	rm -f a.out [Ee]rrs mklog core ${PROG} ${OBJS} ${CLEANFILES}
.endif

STDCLEANDIR: PROGSUBDIR
	rm -f a.out [Ee]rrs mklog core ${PROG} ${OBJS} ${CLEANFILES}
	rm -f .depend ${.CURDIR}/tags ${MANALL}

# some of the rules involve .h sources, so remove them from mkdep line
STDDEPEND: ${SRCS} PROGSUBDIR
	mkdep ${CFLAGS:M-[ID]*} ${.ALLSRC:M*.c}

STDINSTALL: PROGSUBDIR
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${PROG} ${DESTDIR}${BINDIR}
.if defined(HIDEGAME)
	(cd ${DESTDIR}/usr/games; rm -f ${PROG}; ln -s dm ${PROG}; \
	    chown games.bin ${PROG})
.endif

STDLINT: PROGSUBDIR
	@${LINT} ${LINTFLAGS} ${CFLAGS} ${.ALLSRC} | more 2>&1

STDTAGS: PROGSUBDIR
	tags -f ${.CURDIR}/tags ${.ALLSRC}

.include <bsd.own.mk>
