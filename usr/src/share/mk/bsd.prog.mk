# A Makefile for the BSD source tree.
#
#	@(#)bsd.prog.mk	5.5 (Berkeley) %G%
#

# user defines:
#	BINDIR, BINGRP, BINMODE, BINOWN
#		binary target directory, group, mode, owner
#	CLEANFILES
#		list of files to be removed for the target clean; used,
#		for example, to specify .c's produced from .y's.
#	LDLIB	the list of libraries that the program loads, in the format
#		expected by the loader.
#	LINKS	list of binary links of the form "link target link target";
#		for example, "/bin/[ /usr/bin/test" would link /bin/[ to
#		/usr/bin/test"; not particularly labor saving, but prevents
#		needing your own install target.
#	MANDIR, MANMODE
#		manual page installation directory, mode
#	MLINKS	list of man page links of the form "link target link target",
#		with some trickiness so the suffix specifies the directory to
#		use.  For example, "a.1 b.2 c.3 d.4" would link ${MANDIR}1/a.0
#		to ${MANDIR}2/b.0 and ${MANDIR}3/c.0 to ${MANDIR}4/d.0.
#	PROG	program name
#	SHAREDSTRINGS
#		objects share strings using ${XSTR}
#	SRCS	list of .c sources if the program has multiple files.
#	SRCLIB	the list of libraries that the program depends on; normally
#		from the LIB* list in this file.
#	STRIP	strip flag
#
# user macros:
#	STDALL	standard all target
#	STDCLEAN
#		standard clean target
#	STDCLEANDIR
#		standard cleandir target
#	STDDEPEND
#		standard depend target
#	STDLINT
#		standard lint target
#	STDINSTALL
#		standard install target
#	STDTAGS	standard tags target

# permit a hierarchy of Makefile include files
.if exists(../Make.include)
.include "../Make.include"
.endif

# name of the dependency file
DEPENDFILE=	.depend

# standard libraries
LIBC=		/lib/libc.a
LIBCOMPAT=	/usr/lib/libcompat.a
LIBDES=		/usr/lib/libdes.a
LIBKRB=		/usr/lib/libkrb.a
LIBMATH=	/usr/lib/libm.a
LIBUTIL=	/usr/lib/libutil.a

# read-only version of standard .c.o rule
READONLY: .USE
	${CC} ${CFLAGS} -c -R ${.IMPSRC}

# if the user defines SHAREDSTRINGS, they want objects to share strings.
# Turn off parallel makes (the strings file is single threaded) and
# rewrite the .c.o rule to use XSTR to build the objects.
.if defined(SHAREDSTRINGS)
.NOTPARALLEL:
XSTR=/usr/bin/xstr
.c.o:
	${CC} -E ${CFLAGS} ${.IMPSRC} | ${XSTR} -c -
	@${CC} ${CFLAGS} -c x.c -o ${.TARGET}
	@rm -f x.c
.endif

# the default target.
.MAIN: all

# manual pages -- if the Makefile doesn't specify one, assume that they're
# named in a standard way, i.e. it's in section 1 with the same name as the
# program.
MANALL=	${MAN1} ${MAN2} ${MAN3} ${MAN4} ${MAN5} ${MAN6} ${MAN7} ${MAN8}

# In the BSD source tree, each program Makefile should specify PROG, the
# name of the program.
all: ${PROG}

.if defined(SRCS)

# if the program is composed of several object modules, the modules are
# the list of sources with the .o's translated to .c's.
OBJS=	${SRCS:.c=.o}

.if !defined(MAN1) && !defined(MAN2) && !defined(MAN3) && !defined(MAN4) && \
	!defined(MAN5) && !defined(MAN6) && !defined(MAN7) && !defined(MAN8)
MAN1=	${PROG}.0
.endif

${PROG}: ${OBJS} ${LIBC} ${SRCLIB}
	${CC} ${LDFLAGS} -o ${.TARGET} ${OBJS} ${LDLIB}

.depend: ${SRCS}
	@set ${OBJS}
	@for entry in ${.ALLSRC}; do
		echo "$$1:" \
		    `${CPP} -M ${CFLAGS:M-[ID]*} ${.INCLUDES} $$entry`
		shift
	done > ${DEPENDFILE}

${OBJS}: ${.PREFIX}.c

.else

SRCS= ${PROG}.c

.if !defined(MAN1) && !defined(MAN2) && !defined(MAN3) && !defined(MAN4) && \
	!defined(MAN5) && !defined(MAN6) && !defined(MAN7) && !defined(MAN8)
MAN1=	${PROG}.0
.endif

${PROG}: ${SRCS} ${LIBC} ${SRCLIB}
	${CC} ${CFLAGS} -o ${.TARGET} ${SRCS} ${LDLIB}

.depend: ${SRCS}
	echo "${PROG}:" \
	    `${CPP} -M ${CFLAGS:M-[ID]*} ${.INCLUDES} ${.ALLSRC}` \
		    > ${DEPENDFILE}

.endif	# defined(SRCS)

# standard targets
depend: .depend

STDCLEAN: .USE
	rm -f a.out Errs errs mklog core ${CLEANFILES} ${PROG} ${OBJS}

clean: STDCLEAN

STDCLEANDIR: .USE
	rm -f ${MANALL} ${TAGSFILE} ${DEPENDFILE}

cleandir: clean STDCLEANDIR

LINTFLAGS=-chapbx
STDLINT: .USE
	lint ${LINTFLAGS} ${CFLAGS} ${.ALLSRC}

lint: ${SRCS} STDLINT

TAGSFILE=tags
STDTAGS: .USE
	ctags ${.ALLSRC}

tags: ${SRCS} STDTAGS

MANDIR?=	/usr/share/man/cat
MANMODE?=	444
STRIP?=		-s
BINMODE?=	755
BINOWN?=	bin
BINGRP?=	bin

# install target -- creates manual pages, then installs the binaries,
# links, manual pages, and manual page links.
STDINSTALL: .USE
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${PROG} ${DESTDIR}${BINDIR}
.if defined(MAN1)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN1} \
	    ${DESTDIR}${MANDIR}1
.endif
.if defined(MAN2)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN2} \
	    ${DESTDIR}${MANDIR}2
.endif
.if defined(MAN3)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN3} \
	    ${DESTDIR}${MANDIR}3
.endif
.if defined(MAN4)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN4} \
	    ${DESTDIR}${MANDIR}4
.endif
.if defined(MAN5)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN5} \
	    ${DESTDIR}${MANDIR}5
.endif
.if defined(MAN6)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN6} \
	    ${DESTDIR}${MANDIR}6
.endif
.if defined(MAN7)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN7} \
	    ${DESTDIR}${MANDIR}7
.endif
.if defined(MAN8)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN8} \
	    ${DESTDIR}${MANDIR}8
.endif
.if defined(LINKS)
	@set ${LINKS}; \
	while test $$# -ge 2; do \
		t=$$1; \
		shift; \
		l=$$1; \
		shift; \
		echo $$l -\> $$t; \
		rm -f $$t; \
		ln ${DESTDIR}$$l ${DESTDIR}$$t; \
	done; true
.endif	# LINKS
.if defined(MLINKS)
	@set ${MLINKS}; \
	while test $$# -ge 2; do \
		name=$$1; \
		shift; \
		dir=${MANDIR}`expr $$name : '[^\.]*\.\(.*\)'`; \
		t=${DESTDIR}$${dir}/`expr $$name : '\([^\.]*\)'`.0; \
		name=$$1; \
		shift; \
		dir=${MANDIR}`expr $$name : '[^\.]*\.\(.*\)'`; \
		l=${DESTDIR}$${dir}/`expr $$name : '\([^\.]*\)'`.0; \
		echo $$l -\> $$t; \
		rm -f $$t; \
		ln $$l $$t; \
	done; true
.endif	# MLINKS

install: ${MANALL} STDINSTALL
