#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
# make file for intermachine communications package
#
# Files are:
#	/etc/remote		remote host description file
#	/etc/phones		phone number file, owned by ${OWNER} and
#				  mode 6??
#	${ADM}/aculog		ACU accounting file, owned by ${OWNER} and
#				  mode 6?? {if ACULOG defined}
# Presently supports:
#	BIZCOMP
#	DEC DF02-AC, DF03-AC
#	DEC DN-11/Able Quadracall
#	HAYES and Hayes emulators
#	USR COURIER (2400 baud)
#	VENTEL 212+
#	VADIC 831 RS232 adaptor
#	VADIC 3451
# (drivers are located in aculib.a)
#
# Configuration defines:
#	DF02, DF03, DN11	ACU's supported
#	  BIZ1031, BIZ1022, VENTEL, V831, V3451, HAYES, COURIER
#	ACULOG			turn on tip logging of ACU use
#	PRISTINE		no phone #'s put in ACU log file
#	CONNECT			worthless command
#	DEFBR			default baud rate to make connection at
#	DEFFS			default frame size for FTP buffering of
#				writes on local side
#	BUFSIZ			buffer sizing from stdio, must be fed
#				explicitly to remcap.c if not 1024

ADM=	usr/adm
OWNER=	uucp
GROUP=	daemon
CONFIG=	-DV831 -DVENTEL -DHAYES -DCOURIER
CFLAGS=	-O -DDEFBR=1200 -DDEFFS=BUFSIZ -DACULOG -DPRISTINE
LDFLAGS=
OBJS=	acu.o cmds.o cmdtab.o cu.o hunt.o \
	log.o partab.o remote.o tip.o tipout.o value.o vars.o
SPECIAL=acutab.c remcap.c uucplock.c
DRIVERS=aculib/*.c
SOURCES=acu.c acutab.c cmds.c cmdtab.c cu.c hunt.c \
	log.c partab.c remote.c tip.c tipout.c value.c vars.c \
	${SPECIAL} ${DRIVERS}
CXREF=	/usr/ucb/ctags -xw

tip:	${OBJS} acutab.o remcap.o uucplock.o aculib/aculib.a
	${CC} -o tip ${LDFLAGS} ${OBJS} acutab.o remcap.o uucplock.o \
		aculib/aculib.a

${OBJS}: tip.h

remcap.o: remcap.c
	${CC} -c -DBUFSIZ=1024 ${CFLAGS} remcap.c

depend:

# acutab is configuration dependent, and so depends on the makefile
acutab.o: Makefile
acutab.o: acutab.c
	${CC} -c ${CFLAGS} ${CONFIG} acutab.c

# remote.o depends on the makefile because of DEFBR and DEFFS
remote.o: Makefile

# log.o depends on the makefile because of ACULOG
log.o:	Makefile

aculib/aculib.a: ${DRIVERS}
	cd aculib; make ${MFLAGS}

install: tip
	install -s -m 4711 -o ${OWNER} -g ${GROUP} tip ${DESTDIR}/usr/bin/tip
	rm -f ${DESTDIR}/usr/bin/cu
	ln ${DESTDIR}/usr/bin/tip ${DESTDIR}/usr/bin/cu
#	cp /dev/null ${DESTDIR}/${ADM}/aculog
#	chown ${OWNER} ${DESTDIR}/${ADM}/aculog
#	chmod 600 ${DESTDIR}/${ADM}/aculog
#	@echo "create /etc/remote and /etc/phones"

tags:	${SOURCES} tip.h
	ctags -w ${SOURCES} tip.h

clean:
	cd aculib; make ${MFLAGS} clean
	rm -f tip cu *.o

print:
	@pr README TODO makefile
	@size *.o | pr -h SIZES
	@${CXREF} tip.h ${SOURCES} | pr -h XREF
	@ls -l | pr 
	@pr tip.h ${SOURCES}

lint:
	-lint -hbacvx ${CFLAGS} ${SOURCES}
