LIBDIR=		/usr/lib
LINTLIBDIR=	/usr/libdata/lint
SORT=		/usr/bin/sort
CTAGS=		/usr/bin/ctags

.MAIN: all

DEPENDFILE=	.depend
.if exists(.depend)
.include ".depend"
.endif

# minor mods to prefer .s to a .c, and add .po
.SUFFIXES:
.SUFFIXES: .out .a .ln .o .po .s .c .F .f .e .r .y .l .cl .p .h \
                .8 .7 .6 .5 .4 .3 .2 .1 .0
.c.o:
	${CC} ${CFLAGS} -c ${.IMPSRC} 
	@${LD} -x -r ${.TARGET}
	@${MV} a.out ${.TARGET}

.c.po:
	${CC} -p ${CFLAGS} -c ${.IMPSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@${MV} a.out ${.TARGET}

.s.o:
	${CPP} -E ${CFLAGS:M-[ID]*} ${AINC} ${.IMPSRC} | \
	    ${AS} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@${MV} a.out ${.TARGET}

.s.po:
	${CPP} -E -DPROF ${CFLAGS:M-[ID]*} ${AINC} ${.IMPSRC} | \
	    ${AS} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@${MV} a.out ${.TARGET}

MANALL=	${MAN1} ${MAN2} ${MAN3} ${MAN4} ${MAN5} ${MAN6} ${MAN7} ${MAN8}

all: ranlib llib-l${LIB}.ln ${MANALL}

ranlib: reorder
	ranlib lib${LIB}.a
	ranlib lib${LIB}_p.a

STDREORDER: lib${LIB}.a lib${LIB}_p.a .USE

reorder: STDREORDER

OBJS=	${SRCS:S/.c$/.o/g:S/.f$/.o/g:S/.s$/.o/g}
lib${LIB}.a:: ${OBJS}
	@echo building standard ${LIB} library
	@ar cr lib${LIB}.a `lorder ${OBJS} | tsort`

POBJS=	${OBJS:.o=.po}
lib${LIB}_p.a:: ${POBJS}
	@echo building profiled ${LIB} library
	@ar cr lib${LIB}_p.a `lorder ${POBJS} | tsort`

llib-l${LIB}.ln: ${SRCS}
	lint -C${LIB} ${CFLAGS} ${.ALLSRC:M*.c}

clean: STDCLEAN
STDCLEAN: .USE
	rm -f a.out Errs errs mklog core ${CLEANFILES} ${OBJS} ${POBJS} \
	    profiled/*.o lib${LIB}.a lib${LIB}_p.a llib-l${LIB}.ln

cleandir: clean STDCLEANDIR
STDCLEANDIR: .USE
	rm -f ${MANALL} ${TAGSFILE} ${DEPENDFILE}

depend: .depend
.depend: STDDEPEND
STDDEPEND: ${SRCS} .USE
	mkdep ${CFLAGS:M-[ID]*} ${.ALLSRC}

tags: ${SRCS} STDTAGS
STDTAGS: .USE
	${CTAGS} ${.ALLSRC:M*.c}
	${SED} -e 's;../gen/;/usr/src/lib/libc/gen/;' \
	    -e 's;../compat-43/;/usr/src/lib/libc/gen/;' \
	    < tags > tags.tmp
.if !empty(SRCS:M*.s)
	egrep -o "^ENTRY(.*)|^SYSCALL(.*)" ${.ALLSRC:M*.s} | sed \
	"s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \`pwd\`/\1 /^\2(\3\4$$/;" \
	>> tags.tmp
	${SORT} tags.tmp -o tags.tmp
.endif
	${MV} tags.tmp tags

.include <bsd.own.mk>

install: STDINSTALL
STDINSTALL: MANINSTALL .USE
	${INSTALL} -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} lib${LIB}.a \
	    ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}${LIBDIR}/lib${LIB}.a
	${INSTALL} -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    lib${LIB}_p.a ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}/usr/lib/lib${LIB}_p.a
	${INSTALL} -c -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    llib-l${LIB}.ln ${DESTDIR}${LINTLIBDIR}
