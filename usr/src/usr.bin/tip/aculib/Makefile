#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	6/18/87
#
# make file for tip device drivers
#
# Current drivers:
#	BIZCOMP
#	USR COURIER (2400 baud)
#	DEC DF02-AC, DF03-AC
#	DEC DN-11/Able Quadracall
#	HAYES and Hayes emulators
#	VENTEL 212+ (w/o echo)
#	VADIC 831 RS232 adaptor
#	VADIC 3451
CFLAGS=	-O -I..
LIBC=	/lib/libc.a
SRCS=	biz22.c biz31.c courier.c df.c dn11.c hayes.c ventel.c v831.c v3451.c
OBJS=	biz22.o biz31.o courier.o df.o dn11.o hayes.o ventel.o v831.o v3451.o

all: aculib.a

aculib.a: ${OBJS}
	ar cu aculib.a ${OBJS}
	ranlib aculib.a

clean: FRC
	rm -f ${OBJS} core aculib.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

install:
FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

biz22.o: biz22.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
biz22.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
biz22.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
biz22.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
biz22.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
biz22.o: /usr/include/errno.h
biz31.o: biz31.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
biz31.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
biz31.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
biz31.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
biz31.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
biz31.o: /usr/include/errno.h
courier.o: courier.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
courier.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
courier.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
courier.o: /usr/include/signal.h /usr/include/machine/trap.h
courier.o: /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
courier.o: /usr/include/setjmp.h /usr/include/errno.h /usr/include/stdio.h
courier.o: /usr/include/sys/time.h /usr/include/time.h
df.o: df.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
df.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
df.o: /usr/include/sys/ttydev.h /usr/include/signal.h
df.o: /usr/include/machine/trap.h /usr/include/stdio.h /usr/include/pwd.h
df.o: /usr/include/ctype.h /usr/include/setjmp.h /usr/include/errno.h
dn11.o: dn11.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
dn11.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
dn11.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
dn11.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
dn11.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
dn11.o: /usr/include/errno.h
hayes.o: hayes.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
hayes.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
hayes.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
hayes.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
hayes.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
hayes.o: /usr/include/errno.h
ventel.o: ventel.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
ventel.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
ventel.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ventel.o: /usr/include/signal.h /usr/include/machine/trap.h
ventel.o: /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
ventel.o: /usr/include/setjmp.h /usr/include/errno.h
v831.o: v831.c /usr/include/sys/time.h /usr/include/time.h ../tip.h
v831.o: /usr/include/sys/types.h /usr/include/sys/file.h /usr/include/sgtty.h
v831.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
v831.o: /usr/include/sys/ttydev.h /usr/include/signal.h
v831.o: /usr/include/machine/trap.h /usr/include/stdio.h /usr/include/pwd.h
v831.o: /usr/include/ctype.h /usr/include/setjmp.h /usr/include/errno.h
v3451.o: v3451.c ../tip.h /usr/include/sys/types.h /usr/include/sys/file.h
v3451.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
v3451.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
v3451.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
v3451.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/setjmp.h
v3451.o: /usr/include/errno.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
