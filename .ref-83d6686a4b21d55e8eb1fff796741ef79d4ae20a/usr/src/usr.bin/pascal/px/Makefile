#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.7 (Berkeley) %G%
#
SCCSID = "@(#)Makefile 5.7 %G%"

DESTDIR =
BINDIR = /usr/bin
LIBDIR = /usr/lib
SRCDIR = ../src
LIBPCDIR = ../../../lib/libpc
CC = cc
CFLAGS = -O ${INCLUDES}
INCLUDES = -I${SRCDIR} -I${LIBPCDIR}
LINTFLAGS = -hbxc
RM = rm -f

PSHDR =	opc.c pic.c

PXHDR =	vars.h machdep.h

PXSRC =	int.c interp.c utilities.c

PXOBJ =	int.o interp.o utilities.o

PXUTL =	version.c

.c.o:
	${CC} ${CFLAGS} -c $*.c

all px: Version.c ${PXOBJ}
	${CC} ${CFLAGS} -o px Version.c ${PXOBJ} -lpc -lm

lint: Version.c ${PXSRC}
	lint ${LINTFLAGS} ${INCLUDES} Version.c ${PXSRC} -lpc -lm

Version.c: version.c
	${CC} ${CFLAGS} -o version version.c
	./version >Version.c
	${RM}  version

h02opcs.h: ${SRCDIR}/OPnames.h opc.c
	${CC} ${CFLAGS} opc.c -o opc
	./opc >h02opcs.h
	${RM} opc

install: px
	install -s px ${DESTDIR}/usr/bin/px

sources: ${PXHDR} ${PXSRC} ${PXUTL}
${PSHDR}:
	ln -s ${SRCDIR}/$@ $@

cleandir clean:
	${RM} *.o *.s px version opc* pic*
	${RM} h02opcs.h errs lpr core tmp

grind: sources
	${BINDIR}/ctags -v *.h *.c | sort -f >index
	${BINDIR}/vgrind -t -x index >lpr
	${CC} ${CFLAGS} -o pic pic.c
	${BINDIR}/vgrind -t -n READ_ME Makefile lst >>lpr
	${BINDIR}/vgrind -t ${PXSRC} >>lpr
	./pic >lst
	${BINDIR}/vpr lst
	${BINDIR}/vpr -t lpr
	${RM} index pic lst

depend:	sources
	ls *.c | sed 's;^\(.*\)\.c;\1.o: \1.c;' >.depend
	grep '^#[ 	]*include' *.c | sed \
		-e '/<.*>/d' \
		-e 's;:[^"]*"\([^"]*\)".*;: \1;' \
		-e 's/\.c/\.o/' \
		-e 's;: libpc.h;: ${LIBPCDIR}/libpc.h;' \
		-e 's;: objfmt.h;: ${SRCDIR}/objfmt.h;' \
		-e 's;: OPnames.h;: ${SRCDIR}/OPnames.h;' >>.depend

