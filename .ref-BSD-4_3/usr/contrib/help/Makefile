#
# Copyright (c) 1983 Regents of the University of California.
#
#	@(#)Makefile	1.2	(Berkeley)	9/10/85
#
CFLAGS=	-O
DESTDIR=
BINDIR=/usr/new
PROG=help

all:	${PROG}

install: all
	install -s ${PROG} ${DESTDIR}${BINDIR}/${PROG}
	install -c -m 444 help.n ${DESTDIR}/usr/man/mann/help.n

clean:
	rm -f ${PROG} ${PROG}.o core a.out errs

${PROG}:	${PROG}.c
	${CC} ${CFLAGS} -o ${PROG} ${PROG}.c

${PROG}: /usr/include/sys/types.h
${PROG}: /usr/include/sys/stat.h
${PROG}: /usr/include/sys/dir.h
${PROG}: /usr/include/setjmp.h
${PROG}: /usr/include/signal.h
${PROG}: /usr/include/ctype.h
${PROG}: /usr/include/stdio.h
