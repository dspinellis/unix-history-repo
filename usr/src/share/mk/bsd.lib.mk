.include <bsd.global.mk>

.if exists(${.CURDIR}/../Makefile.inc)
.include "${.CURDIR}/../Makefile.inc"
.endif

.MAIN: all

# minor mods to prefer .s to a .c, and add .po
.SUFFIXES:
.SUFFIXES: .out .a .ln .o .po .s .c .F .f .e .r .y .l .cl .p .h \
                .8 .7 .6 .5 .4 .3 .2 .1 .0
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

STDCLEAN:
	rm -f a.out Errs errs mklog core ${CLEANFILES} ${OBJS} ${POBJS} \
	    profiled/*.o lib${LIB}.a lib${LIB}_p.a llib-l${LIB}.ln

STDCLEANDIR:
	rm -f ${MANALL} tags .depend

STDDEPEND: ${SRCS} .USE
	mkdep ${CFLAGS:M-[ID]*} ${AINC} ${.ALLSRC}

STDINSTALL:
	install -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} lib${LIB}.a \
	    ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}${LIBDIR}/lib${LIB}.a
	install -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    lib${LIB}_p.a ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}/usr/lib/lib${LIB}_p.a
	install -c -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    llib-l${LIB}.ln ${DESTDIR}${LINTLIBDIR}

STDLINT:

STDTAGS: .USE
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

.include <bsd.own.mk>
.include <bsd.stdtarg.mk>
