#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1	(Berkeley)	%G%
#
# optional flags are: MEASURE TESTING

CFLAGS= -O -DMEASURE -DTESTING
DESTDIR= /etc
MACHINE= vax
LIBS=
PROGRS=	timed timedc

HDRDS= 	/usr/include/protocols/timed.h globals.h

SRCDS=	acksend.c broadcast.c candidate.c correct.c \
	master.c networkdelta.c readmsg.c slave.c timed.c

OBJDS=	acksend.o broadcast.o candidate.o correct.o \
	master.o networkdelta.o readmsg.o slave.o timed.o

HDRCS=	/usr/include/protocols/timed.h timedc.h

SRCCS=	cmds.c cmdtab.c timedc.c

OBJCS=	cmds.o cmdtab.o timedc.o

COMMS=	byteorder.c measure.c ${MACHINE}/cksum.c

COMMO=	byteorder.o measure.o ${MACHINE}/cksum.o

all:	${PROGRS}

${SRCDS}: ${HDRDS}

${SRCCS}: ${HDRCS}

${COMMS}: ${HDRDS}

${MACHINE}/cksum.o:
	cc ${CFLAGS} ${LIBS} -c $<
	mv cksum.o ${MACHINE}

timed: 	${OBJDS} ${COMMO}
	cc ${CFLAGS} ${OBJDS} ${COMMO} ${LIBS} -o timed 

timedc: ${OBJCS} ${COMMO}
	cc ${CFLAGS} ${OBJCS} ${COMMO} ${LIBS} -o timedc

install: ${PROGRS}
	install -s timed ${DESTDIR}
	install -s -m 4511 timedc ${DESTDIR}

clean:
	rm -f a.out core ${OBJDS} ${OBJCS} ${COMMO} ${PROGRS}

lint:
	lint ${SRCDS} ${COMMS}
	lint ${SRCCS} ${COMMS}
