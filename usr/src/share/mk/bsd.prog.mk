
MAKE=	pmake	# for now...
CPP=	newcpp

# libraries used for dependency lines
LIBUTIL=	/usr/lib/libutil.a
LIBMATH=	/usr/lib/libm.a
LIBKRB=		/usr/lib/libkrb.a
LIBDES=		/usr/lib/libdes.a
LIBCOMPAT=	/usr/lib/libcompat.a

# any specified manual pages -- if none have been specified at this point,
# we make assumptions about what they are and what they're called.
MANALL=	${MAN1} ${MAN2} ${MAN3} ${MAN4} ${MAN5} ${MAN6} ${MAN7} ${MAN8}

# user defines:
#	SUBDIR -- the list of subdirectories to be processed
#
# if SUBDIR is set, we're in a makefile that processes subdirectories;
# for all the standard targets, change to the subdirectory and make the
# target.  If making one of the subdirectories, change to it and make
# the target "all".  Machine dependent subdirectories take precedence
# over standard subdirectories.
.if defined(SUBDIR)
all depend clean cleandir lint install tags:
	@for entry in ${SUBDIR}; do
		(echo "====> $$entry"
		if test -d $${entry}.${MACHINE}; then
			cd $${entry}.${MACHINE}
		else
			cd $${entry}
		fi
		${MAKE} ${.TARGET})
	done

${SUBDIR}:
	@if test -d ${.TARGET}.${MACHINE}; then
		cd ${.TARGET}.${MACHINE}
	else
		cd ${.TARGET}
	fi
	${MAKE} all

.else	# !SUBDIR

# user defines:
#	SHAREDSTRINGS -- boolean variable, if sharing strings in objects.
#
# if SHAREDSTRINGS is defined, use XSTR to build objects.
#
.if defined(SHAREDSTRINGS)
.NOTPARALLEL:
XSTR=	xstr
.c.o:
	${CC} -E ${.INCLUDES} ${CFLAGS} ${.IMPSRC} | ${XSTR} -c -
	@${CC} ${.INCLUDES} ${CFLAGS} -c x.c -o ${.TARGET}
	@rm -f x.c
.endif

# the default target is all
.MAIN: all

# user defines:
#	PROGC	-- the name of a program composed of a single source module
#	PROGO	-- the name of a program composed of several object modules
#	SRCLIB	-- the list of libraries that the program depends on;
#		   normally from the LIB* list at the top of this file.
#	LDLIB	-- the list of libraries that the program loads, in the
#		   format expected by the loader.

all: ${PROGC} ${PROGO}

# if the program is composed of a single source module, that module is
# C source with the same name as the program.  If no manual pages have
# been defined, it's in section 1 with the same name as the program.
.if defined(PROGC)
SRCS=	${PROGC}.c

.if !defined(MAN1) && !defined(MAN2) && !defined(MAN3) && !defined(MAN4) && !defined(MAN5) && !defined(MAN6) && !defined(MAN7) && !defined(MAN8)
MAN1=	${PROGC}.0
.endif

${PROGC}: ${SRCS} ${LIBC} ${SRCLIB}
	${CC} ${CFLAGS} -o ${.TARGET} ${SRCS} ${LDLIB}

.depend: ${SRCS}
	set -
	${CPP} -M ${CFLAGS:M-[ID]*} ${.INCLUDES} ${.ALLSRC} |
		sed "s/^/${PROGC}: /" > ${DEPENDFILE}.new
	mv ${DEPENDFILE}.new ${DEPENDFILE}

.endif	# PROGC

# if the program is composed of several object modules, the modules are
# the list of sources with the .o's translated to .c's.  If no manual
# pages have been defined, it's in section 1 with the same name as the
# program.  Objects depend on their C source counterparts.
.if defined(PROGO)

OBJS=	${SRCS:.c=.o}

.if !defined(MAN1) && !defined(MAN2) && !defined(MAN3) && !defined(MAN4) && !defined(MAN5) && !defined(MAN6) && !defined(MAN7) && !defined(MAN8)
MAN1=	${PROGO}.0
.endif

${PROGO}: ${OBJS} ${LIBC} ${SRCLIB}
	${CC} ${LDFLAGS} -o ${.TARGET} ${OBJS} ${LDLIB}

.depend: ${SRCS}
	set -
	set ${OBJS}
	rm -f ${DEPENDFILE}.new
	for entry in ${.ALLSRC}; do
		${CPP} -M ${CFLAGS:M-[ID]*} ${.INCLUDES} $$entry |
			sed "s/^/$$1: /" >> ${DEPENDFILE}.new
		shift
	done
	mv ${DEPENDFILE}.new ${DEPENDFILE}

${OBJS}: ${.PREFIX}.c

.endif	# PROGO

depend: .depend

# user defines:
#	CLEANFILES	-- list of files to be removed for the target clean;
#			   used, for example, to specify .c's produced from
#			   .y's.
JUNKFILES=	a.out Errs errs mklog core
clean:
	rm -f ${PROGC} ${PROGO} ${OBJS} ${CLEANFILES} ${JUNKFILES}

TAGSFILE=	tags
cleandir: clean
	rm -f ${MANALL} ${TAGSFILE} ${DEPENDFILE}

LINTFLAGS=	-chapbx
lint: ${SRCS}
	lint ${LINTFLAGS} ${CFLAGS} ${.ALLSRC}

tags: ${SRCS}
	ctags ${.ALLSRC}

# user defines:
#	MDIR	-- default manual page installation directory
#	MANMODE	-- default manual page installation mode
#	STRIP	-- default strip flag
#	BINMODE	-- default binary installation mode
#	BINOWN	-- default binary owner
#	BINGRP	-- default binary group
MDIR?=		/usr/share/man/cat
MANMODE?=	444
STRIP?=		-s
BINMODE?=	755
BINOWN?=	bin
BINGRP?=	bin

# install target -- creates manual pages, then installs the binaries,
# manual pages, and manual page links.
install: ${MANALL}
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${PROGC} ${PROGO} ${DESTDIR}${DIR}
.if defined(MAN1)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN1} \
	    ${DESTDIR}${MDIR}1
.endif
.if defined(MAN2)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN2} \
	    ${DESTDIR}${MDIR}2
.endif
.if defined(MAN3)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN3} \
	    ${DESTDIR}${MDIR}3
.endif
.if defined(MAN4)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN4} \
	    ${DESTDIR}${MDIR}4
.endif
.if defined(MAN5)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN5} \
	    ${DESTDIR}${MDIR}5
.endif
.if defined(MAN6)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN6} \
	    ${DESTDIR}${MDIR}6
.endif
.if defined(MAN7)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN7} \
	    ${DESTDIR}${MDIR}7
.endif
.if defined(MAN8)
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${MAN8} \
	    ${DESTDIR}${MDIR}8
.endif
# user defines:
#	LINKS	-- list of binary links of the form "link target
#		   link target"; for example, "/bin/[ /usr/bin/test"
#		   would link /bin/[ to /usr/bin/test"; not particularly
#		   labor saving, but prevents needing your own install
#		   target.
#	MLINKS	-- list of manual page linkes of the form "link target
#		   link target"; for example, "a.1 b.2 c.3 d.4" would
#		   link ${MDIR}1/a.0 to ${MDIR}2/b.0 and ${MDIR}3/c.0
#		   to ${MDIR}4/d.0.
.if defined(LINKS)
	@set ${LINKS}
	@while :; do
		if `test $$# -lt 2`; then
			break;
		fi
		t=$$1
		shift
		l=$$1
		shift
		echo $$l -\> $$t
		rm -f $$t
		ln ${DESTDIR}$$l ${DESTDIR}$$t
	done
.endif	# LINKS
.if defined(MLINKS)
	@set ${MLINKS}
	@while :; do
		if `test $$# -lt 2`; then
			break;
		fi
		name=$$1
		shift
		dir=${MDIR}`expr $$name : '[^\.]*\.\(.*\)'`
		t=${DESTDIR}$${dir}/`expr $$name : '\([^\.]*\)'`.0
		name=$$1
		shift
		dir=${MDIR}`expr $$name : '[^\.]*\.\(.*\)'`
		l=${DESTDIR}$${dir}/`expr $$name : '\([^\.]*\)'`.0
		echo $$l -\> $$t
		rm -f $$t
		ln $$l $$t
	done
.endif	# MLINKS
.endif	# SUBDIR
