#	@(#)bsd.lib.mk	5.26 (Berkeley) 5/2/91
#
# $Log: bsd.lib.mk,v $
# Revision 1.10  1993/08/11  03:15:20  alm
# added rules .f.po (and .f.o) from Jonas.
#
# Revision 1.9  1993/08/05  18:45:53  nate
# Removed the ranlib statements from before the install (since it's done
# after the install as well), and changed ranlib -> ${RANLIB}
#
# Revision 1.8  1993/08/03  20:57:34  nate
# Fixed macros so that you can do a
# make maninstall at all times and have it not blow up
#
# Revision 1.7  1993/07/23  20:44:38  nate
# Fixed a boo-boo and made the NOMAN environment variable work correctly with
# both programs and libraries.
#
# Revision 1.6  1993/07/09  00:38:35  jkh
# Removed $History$ line from hell (no leading #).
#
# Revision 1.5  1993/07/08  12:17:07  paul
# Removed the core.* before disaster strikes.
# I removed core as well since it's pretty redundant.
#
# Revision 1.4  1993/07/07  21:42:45  nate
# Cleaned up header files and added core.* to clean directives
#
# Revision 1.3  1993/07/02  06:44:30  root
# New manual page system
#
# Revision 1.2  1993/06/17  02:01:11  rgrimes
# Make clean in src/lib/libc failed due to too many arguments to /bin/sh,
# this was fixed for make cleandir in the patchkit, this fixes it for
# make clean.
#

.if exists(${.CURDIR}/../Makefile.inc)
.include "${.CURDIR}/../Makefile.inc"
.endif

LIBDIR?=	/usr/lib
LINTLIBDIR?=	/usr/libdata/lint
LIBGRP?=	bin
LIBOWN?=	bin
LIBMODE?=	444

STRIP?=	-s

BINGRP?=	bin
BINOWN?=	bin
BINMODE?=	555

.MAIN: all

# prefer .s to a .c, add .po, remove stuff not used in the BSD libraries
#.SUFFIXES: .out .o .po .s .c .cc .cxx .C .f .y .l
.SUFFIXES: .out .o .po .s .c .f .y .l

.c.o:
	${CC} ${CFLAGS} -c ${.IMPSRC} 
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.c.po:
	${CC} -p ${CFLAGS} -c ${.IMPSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}

#.cc.o .cxx.o .C.o:
#	${CXX} ${CXXFLAGS} -c ${.IMPSRC} 
#	@${LD} -x -r ${.TARGET}
#	@mv a.out ${.TARGET}

#.cc.po .C.po .cxx.o:
#	${CXX} -p ${CXXFLAGS} -c ${.IMPSRC} -o ${.TARGET}
#	@${LD} -X -r ${.TARGET}
#	@mv a.out ${.TARGET}

.f.o:
	${FC} ${RFLAGS} -o ${.TARGET} -c ${.IMPSRC} 
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.f.po:
	${FC} -p ${RFLAGS} -o ${.TARGET} -c ${.IMPSRC} 
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

.if !defined(NOPROFILE)
_LIBS=lib${LIB}.a lib${LIB}_p.a
.else
_LIBS=lib${LIB}.a
.endif

all: ${_LIBS} # llib-l${LIB}.ln

OBJS+=	${SRCS:N*.h:R:S/$/.o/g}

lib${LIB}.a:: ${OBJS}
	@echo building standard ${LIB} library
	@rm -f lib${LIB}.a
	@${AR} cTq lib${LIB}.a `lorder ${OBJS} | tsort` ${LDADD}
	${RANLIB} lib${LIB}.a

POBJS+=	${OBJS:.o=.po}
lib${LIB}_p.a:: ${POBJS}
	@echo building profiled ${LIB} library
	@rm -f lib${LIB}_p.a
	@${AR} cTq lib${LIB}_p.a `lorder ${POBJS} | tsort` ${LDADD}
	${RANLIB} lib${LIB}_p.a

llib-l${LIB}.ln: ${SRCS}
	${LINT} -C${LIB} ${CFLAGS} ${.ALLSRC:M*.c}

.if !target(clean)
clean:
	rm -f a.out Errs errs mklog ${CLEANFILES} ${OBJS}
	rm -f lib${LIB}.a llib-l${LIB}.ln
	rm -f ${POBJS} profiled/*.o lib${LIB}_p.a
.endif

.if !target(cleandir)
cleandir:
	rm -f a.out Errs errs mklog ${CLEANFILES} ${OBJS}
	rm -f lib${LIB}.a llib-l${LIB}.ln
	rm -f ${.CURDIR}/tags .depend
	rm -f ${POBJS} profiled/*.o lib${LIB}_p.a
	cd ${.CURDIR}; rm -rf obj;
.endif

.if !target(depend)
depend: .depend
.depend: ${SRCS}
	rm -f .depend
	files="${.ALLSRC:M*.c}"; \
	if [ "$$files" != "" ]; then \
	  mkdep -a ${MKDEP} ${CFLAGS:M-[ID]*} $$files; \
	fi
#	files="${.ALLSRC:M*.cc} ${.ALLSRC:M*.C} ${.ALLSRC:M*.cxx}"; \
#	if [ "$$files" != "  " ]; then \
#	  mkdep -a ${MKDEP} -+ ${CXXFLAGS:M-[ID]*} $$files; \
#	fi
	@(TMP=/tmp/_depend$$$$; \
	    sed -e 's/^\([^\.]*\).o[ ]*:/\1.o \1.po:/' < .depend > $$TMP; \
	    mv $$TMP .depend)
.endif

.if !target(install)
.if !target(beforeinstall)
beforeinstall:
.endif

realinstall: beforeinstall
	install ${COPY} -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} lib${LIB}.a \
	    ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}${LIBDIR}/lib${LIB}.a
.if !defined(NOPROFILE)
	install ${COPY} -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
	    lib${LIB}_p.a ${DESTDIR}${LIBDIR}
	${RANLIB} -t ${DESTDIR}${LIBDIR}/lib${LIB}_p.a
.endif
#	install ${COPY} -o ${LIBOWN} -g ${LIBGRP} -m ${LIBMODE} \
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
.if !defined(NOMAN)
afterinstall: realinstall maninstall
.else
afterinstall: realinstall
.endif
.endif

.if !target(lint)
lint:
.endif

.if !target(tags)
tags: ${SRCS}
	-cd ${.CURDIR}; ctags -f /dev/stdout ${.ALLSRC:M*.c} | \
	    sed "s;\${.CURDIR}/;;" > tags
.endif

.if !defined(NOMAN)
.include <bsd.man.mk>
.elif !target(maninstall)
maninstall:
.endif

.if !target(obj)
.if defined(NOOBJ)
obj:
.else
obj:
	@cd ${.CURDIR}; rm -rf obj; \
	here=`pwd`; dest=/usr/obj/`echo $$here | sed 's,/usr/src/,,'`; \
	echo "$$here -> $$dest"; ln -s $$dest obj; \
	if test -d /usr/obj -a ! -d $$dest; then \
		mkdir -p $$dest; \
	else \
		true; \
	fi;
.endif
.endif
