
.if exists(${.CURDIR}/../Makefile.inc)
.include "${.CURDIR}/../Makefile.inc"
.endif

.MAIN: all

# prefer .s to a .c, add .po, remove stuff not used in the BSD tree
.SUFFIXES:
.SUFFIXES: .out .o .po .s .c .y .l

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

all: ranlib llib-l${LIB}.ln ${MANALL}

ranlib: lib${LIB}.a lib${LIB}_p.a
	ranlib lib${LIB}.a
	ranlib lib${LIB}_p.a

.if target(reorder)
ranlib: reorder
.endif

OBJS=	${SRCS:S/.c$/.o/g:S/.f$/.o/g:S/.s$/.o/g}
lib${LIB}.a:: ${OBJS}
	@echo building standard ${LIB} library
	@${AR} cr lib${LIB}.a `lorder ${OBJS} | tsort` ${LDADD}

POBJS=	${OBJS:.o=.po}
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
	rm -f ${MANALL} tags .depend
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
	install -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} lib${LIB}.a \
	    ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}${LIBDIR}/lib${LIB}.a
	install -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    lib${LIB}_p.a ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}/usr/lib/lib${LIB}_p.a
	install -c -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    llib-l${LIB}.ln ${DESTDIR}${LINTLIBDIR}

install: afterinstall
afterinstall: realinstall
.endif

.if !target(lint)
lint:
.endif

.if !target(tags)
tags:
	tags ${.ALLSRC:M*.c}
	sed -e 's;../gen/;/usr/src/lib/libc/gen/;' \
	    -e 's;../compat-43/;/usr/src/lib/libc/gen/;' \
	    < tags > tags.tmp
.if !empty(SRCS:M*.s)
	egrep -o "^ENTRY(.*)|^SYSCALL(.*)" ${.ALLSRC:M*.s} | sed \
	"s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \`pwd\`/\1 /^\2(\3\4$$/;" \
	>> tags.tmp
	sort tags.tmp -o tags.tmp
.endif
	mv tags.tmp tags
.endif

.include <bsd.own.mk>
