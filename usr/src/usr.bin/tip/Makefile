#
# Copyright (c) 1988 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
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
CONFIG=	-DV831 -DVENTEL -DHAYES -DCOURIER
CFLAGS=	-O -DDEFBR=1200 -DDEFFS=BUFSIZ -DACULOG -DPRISTINE
LIBC=	/lib/libc.a
SRCS=	acu.c acutab.c cmds.c cmdtab.c cu.c hunt.c log.c partab.c remcap.c \
	remote.c tip.c tipout.c uucplock.c value.c vars.c
OBJS=	acu.o acutab.o cmds.o cmdtab.o cu.o hunt.o log.o partab.o remcap.o \
	remote.o tip.o tipout.o uucplock.o value.o vars.o

all: aculib/aculib.a tip

tip: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} aculib/aculib.a

# acutab is configuration dependent, and so depends on the makefile
acutab.o: Makefile
acutab.o: acutab.c
	${CC} -c ${CFLAGS} ${CONFIG} acutab.c

# remote.o depends on the makefile because of DEFBR and DEFFS
# log.o depends on the makefile because of ACULOG
log.o remote.o: Makefile

aculib/aculib.a: FRC
	cd aculib; make ${MFLAGS}

clean:
	rm -f ${OBJS} core tip
	cd aculib; make ${MFLAGS} clean

cleandir: clean
	rm -f ${MAN} tags .depend
	cd aculib; make ${MFLAGS} cleandir

depend:
	mkdep ${CFLAGS} ${SRCS}
	cd aculib; make ${MFLAGS} depend

install: FRC
	cd aculib; make ${MFLAGS} install
	install -s -o uucp -g daemon -m 4711 tip ${DESTDIR}/usr/bin/tip
	rm -f ${DESTDIR}/usr/bin/cu
	ln ${DESTDIR}/usr/bin/tip ${DESTDIR}/usr/bin/cu

lint:
	lint ${CFLAGS} ${SRCS}

tags:
	ctags ${SRCS}
	cd aculib; make ${MFLAGS} tags
