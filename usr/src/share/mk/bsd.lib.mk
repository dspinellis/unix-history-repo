#	@(#)bsd.lib.mk	5.11 (Berkeley) %G%

.if exists(${.CURDIR}/../Makefile.inc)
.include "${.CURDIR}/../Makefile.inc"
.endif

LIBDIR?=	/usr/lib
LINTLIBDIR?=	/usr/libdata/lint
LIBGRP?=	bin
LIBOWN?=	bin
LIBMODE?=	444

.MAIN: all

# prefer .s to a .c, add .po, remove stuff not used in the BSD libraries
.SUFFIXES:
.SUFFIXES: .out .o .po .s .c .f .y .l .8 .7 .6 .5 .4 .3 .2 .1 .0

.8.0 .7.0 .6.0 .5.0 .4.0 .3.0 .2.0 .1.0:
	nroff -man -h ${.IMPSRC} > ${.TARGET}

.c.o:
	${CC} ${CFLAGS} -c ${.IMPSRC} 
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.c.po:
	${CC} -p ${CFLAGS} -c ${.IMPSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}

.s.o:
	${CPP} -E ${CFLAGS:M-[ID]*} ${AINC} ${.IMPSRC} | \
	    ${AS} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.s.po:
	${CPP} -E -DPROF ${CFLAGS:M-[ID]*} ${AINC} ${.IMPSRC} | \
	    ${AS} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}

MANALL=	${MAN1} ${MAN2} ${MAN3} ${MAN4} ${MAN5} ${MAN6} ${MAN7} ${MAN8}

all: lib${LIB}.a lib${LIB}_p.a ${MANALL}# llib-l${LIB}.ln

OBJS+=	${SRCS:S/.c$/.o/g:S/.f$/.o/g:S/.s$/.o/g}
lib${LIB}.a:: ${OBJS}
	@echo building standard ${LIB} library
	@${AR} cr lib${LIB}.a `lorder ${OBJS} | tsort` ${LDADD}

POBJS+=	${OBJS:.o=.po}
lib${LIB}_p.a:: ${POBJS}
	@echo building profiled ${LIB} library
	@${AR} cr lib${LIB}_p.a `lorder ${POBJS} | tsort` ${LDADD}

llib-l${LIB}.ln: ${SRCS}
	${LINT} -C${LIB} ${CFLAGS} ${.ALLSRC:M*.c}

.if !target(clean)
clean:
	rm -f a.out Errs errs mklog core ${CLEANFILES} ${OBJS} ${POBJS} \
	    profiled/*.o lib${LIB}.a lib${LIB}_p.a llib-l${LIB}.ln
.endif

.if !target(cleandir)
cleandir:
	rm -f a.out Errs errs mklog core ${CLEANFILES} ${OBJS} ${POBJS} \
	    profiled/*.o lib${LIB}.a lib${LIB}_p.a llib-l${LIB}.ln
	rm -f ${MANALL} ${.CURDIR}/tags .depend
.endif

.if !target(depend)
depend: .depend
.depend: ${SRCS}
	mkdep ${CFLAGS:M-[ID]*} ${AINC} ${.ALLSRC}
.endif

.if !target(install)
.if !target(beforeinstall)
beforeinstall:
.endif

realinstall: beforeinstall
	ranlib lib${LIB}.a
	install -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} lib${LIB}.a \
	    ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}${LIBDIR}/lib${LIB}.a
	ranlib lib${LIB}_p.a
	install -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    lib${LIB}_p.a ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}/usr/lib/lib${LIB}_p.a
#	install -c -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
#	    llib-l${LIB}.ln ${DESTDIR}${LINTLIBDIR}
.if defined(LINKS) && !empty(LINKS)
	@set ${LINKS}; \
	while test $$# -ge 2; do \
		l=${DESTDIR}$$1; \
		shift; \
		t=${DESTDIR}$$1; \
		shift; \
		echo $$t -\> $$l; \
		rm -f $$t; \
		ln $$l $$t; \
	done; true
.endif

install: afterinstall
afterinstall: realinstall maninstall
.endif

.if !target(lint)
lint:
.endif

.if !target(tags)
tags:
	cd ${.CURDIR}; ctags -f /dev/stdout ${.ALLSRC:M*.c} | \
	    sed "s;\${.CURDIR}/;;" > tags
.if !empty(SRCS:M*.s)
	cd ${.CURDIR}; egrep -o "^ENTRY(.*)|^SYSCALL(.*)" ${.ALLSRC:M*.s} | \
	    sed \
	    "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \`pwd\`/\1 /^\2(\3\4$$/;" \
	    >> tags; sort -o tags tags
.endif
.endif

.include <bsd.man.mk>
