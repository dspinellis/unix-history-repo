#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	6/18/87
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

remcap.o: remcap.c
	${CC} -c -DBUFSIZ=1024 ${CFLAGS} remcap.c

# acutab is configuration dependent, and so depends on the makefile
acutab.o: Makefile
acutab.o: acutab.c
	${CC} -c ${CFLAGS} ${CONFIG} acutab.c

# remote.o depends on the makefile because of DEFBR and DEFFS
# log.o depends on the makefile because of ACULOG
log.o remote.o: Makefile

aculib/aculib.a: FRC
	cd aculib; make ${MFLAGS}

clean: FRC
	rm -f ${OBJS} core tip
	cd aculib; make ${MFLAGS} clean

depend: FRC
	mkdep ${CFLAGS} ${SRCS}
	cd aculib; make ${MFLAGS} depend

install: FRC
	cd aculib; make ${MFLAGS} install
	install -s -o uucp -g daemon -m 4711 tip ${DESTDIR}/usr/bin/tip
	rm -f ${DESTDIR}/usr/bin/cu
	ln ${DESTDIR}/usr/bin/tip ${DESTDIR}/usr/bin/cu
#	cp /dev/null ${DESTDIR}/${ADM}/aculog
#	chown ${OWNER} ${DESTDIR}/${ADM}/aculog
#	chmod 600 ${DESTDIR}/${ADM}/aculog
#	@echo "create /etc/remote and /etc/phones"

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

acu.o: acu.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
acu.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
acu.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
acu.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
acu.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
acu.o: /usr/include/errno.h
acutab.o: acutab.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
acutab.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
acutab.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
acutab.o: /usr/include/signal.h /usr/include/machine/trap.h
acutab.o: /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
acutab.o: /usr/include/setjmp.h /usr/include/errno.h
cmds.o: cmds.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
cmds.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmds.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmds.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
cmds.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
cmds.o: /usr/include/errno.h
cmdtab.o: cmdtab.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
cmdtab.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmdtab.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/trap.h
cmdtab.o: /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
cmdtab.o: /usr/include/setjmp.h /usr/include/errno.h
cu.o: cu.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
cu.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cu.o: /usr/include/sys/ttydev.h /usr/include/signal.h
cu.o: /usr/include/machine/trap.h /usr/include/stdio.h /usr/include/pwd.h
cu.o: /usr/include/ctype.h /usr/include/setjmp.h /usr/include/errno.h
hunt.o: hunt.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
hunt.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
hunt.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
hunt.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
hunt.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
hunt.o: /usr/include/errno.h
log.o: log.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
log.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
log.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
log.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
log.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
log.o: /usr/include/errno.h
partab.o: partab.c
remcap.o: remcap.c /usr/include/sys/file.h /usr/include/ctype.h
remote.o: remote.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
remote.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
remote.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
remote.o: /usr/include/signal.h /usr/include/machine/trap.h
remote.o: /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
remote.o: /usr/include/setjmp.h /usr/include/errno.h
tip.o: tip.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
tip.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
tip.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tip.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
tip.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
tip.o: /usr/include/errno.h
tipout.o: tipout.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
tipout.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
tipout.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tipout.o: /usr/include/signal.h /usr/include/machine/trap.h
tipout.o: /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
tipout.o: /usr/include/setjmp.h /usr/include/errno.h
uucplock.o: uucplock.c /usr/include/sys/types.h /usr/include/sys/stat.h
uucplock.o: /usr/include/stdio.h
value.o: value.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
value.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
value.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
value.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
value.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
value.o: /usr/include/errno.h
vars.o: vars.c tip.h /usr/include/sys/types.h /usr/include/sys/file.h
vars.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
vars.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vars.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
vars.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
vars.o: /usr/include/errno.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
