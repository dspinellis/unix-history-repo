#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.13 (Berkeley) %G%
#

OBJS=	ftime.o getpw.o gtty.o rand.o reset.o stty.o tell.o times.o \
	utime.o vlimit.o vtimes.o
SRCS=	ftime.c getpw.c gtty.c rand.c stty.c tell.c times.c utime.c \
	vlimit.c vtimes.c
ASRCS=	reset.s
MAN=	ftime.0 getpw.0 rand.0 stty.0 times.0 utime.0 vlimit.0 vtimes.0

# Tahoe specific sources
TSRC=	${SRCS}
TASRC=	${ASRCS}

# Vax specific sources
#VSRC=	${SRCS}
#VASRC=	${ASRCS}

VPATH=	${MACHINE}
FLAGS=	-I${MACHINE} -I../../libc/${MACHINE}
DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}
TAGSFILE=tags
MDIR=	/usr/man/cat3

.SUFFIXES:
.SUFFIXES: .o .s .c .3 .0

.s.o:
	@${CPP} -E ${FLAGS} -DPROF $*.s | ${AS} -o $*.o
	@ld -X -o profiled/$*.o -r $*.o
	${CPP} -E ${FLAGS} $*.s | ${AS} -o $*.o
	@ld -x -r $*.o
	@mv a.out $*.o

.c.o:
	@${CC} -p ${CFLAGS} -c $*.c
	@-ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

clean:
	rm -f *.o profiled/* a.out core

cleandir: clean
	rm -f ${MAN} tags .depend

depend: depend.${MACHINE}

depend.tahoe: ${TSRC}
	mkdep ${CFLAGS} ${TSRC}

depend.vax: ${VSRC}
	mkdep ${CFLAGS} ${VSRC}

tags: tags.${MACHINE}

tags.tahoe: ${TSRC} ${TASRC}
	rm -f ${TAGSFILE}
	cwd=`pwd`; \
	for i in ${TSRC}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done
	(P=`pwd`; \
	egrep "^ENTRY(.*)|^SYSCALL(.*)" ${TASRC} | sed \
"s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 $$P/\1 /^\2(\3\4$$/;" \
	>> ${TAGSFILE})

tags.vax: ${VSRC} ${VASRC}
	rm -f ${TAGSFILE}
	cwd=`pwd`; \
	for i in ${VSRC}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done
	(P=`pwd`; \
	egrep "^ENTRY(.*)|^SYSCALL(.*)" ${VASRC} | sed \
"s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 $$P/\1 /^\2(\3\4$$/;" \
	>> ${TAGSFILE})

install: ${MAN}
	cd ../library; rm -f ${OBJS}
	ln ${OBJS} ../library
	cd ../profiled; rm -f ${OBJS}
	cd profiled; ln ${OBJS} ../../profiled
	install -c -o bin -g bin -m 444 ${MAN} ${MDIR}
	rm -f ${MDIR}/srand.0; ln ${MDIR}/rand.0 ${MDIR}/srand.0
	rm -f ${MDIR}/gtty.0; ln ${MDIR}/stty.0 ${MDIR}/gtty.0
