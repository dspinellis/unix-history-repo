#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.5	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	bsdtcp.c df2.c df12.c dk.c dn.c hys.c hysq.c mic.c nov.c pen.c pnet.c \
	rvmacs.c sy.c unet.c va212.c va811.c va820.c vad.c vent.c vmacs.c \
	hys24.c cds224.c att2224.c
OBJS=	bsdtcp.o df2.o df12.o dk.o dn.o hys.o hysq.o mic.o nov.o pen.o pnet.o \
	rvmacs.o sy.o unet.o va212.o va811.o va820.o vad.o vent.o vmacs.o \
	hys24.o cds224.o att2224.o

all: aculib.a

aculib.a: ${OBJS}
	ar cr aculib.a ${OBJS}
	-ranlib aculib.a 2>&1 | sed '/no symbol table/d'

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

bsdtcp.o: bsdtcp.c ../condevs.h ../uucp.h /usr/include/stdio.h
bsdtcp.o: /usr/include/sysexits.h /usr/include/sys/types.h
bsdtcp.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
bsdtcp.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
bsdtcp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
bsdtcp.o: /usr/include/netdb.h /usr/include/sys/socket.h
bsdtcp.o: /usr/include/netinet/in.h
df2.o: df2.c ../condevs.h ../uucp.h /usr/include/stdio.h
df2.o: /usr/include/sysexits.h /usr/include/sys/types.h
df2.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
df2.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
df2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
df12.o: df12.c ../condevs.h ../uucp.h /usr/include/stdio.h
df12.o: /usr/include/sysexits.h /usr/include/sys/types.h
df12.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
df12.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
df12.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
dk.o: dk.c ../condevs.h ../uucp.h /usr/include/stdio.h /usr/include/sysexits.h
dk.o: /usr/include/sys/types.h /usr/include/sys/timeb.h /usr/include/errno.h
dk.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/sgtty.h
dk.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
dk.o: /usr/include/sys/ttydev.h
dn.o: dn.c ../condevs.h ../uucp.h /usr/include/stdio.h /usr/include/sysexits.h
dn.o: /usr/include/sys/types.h /usr/include/sys/timeb.h /usr/include/errno.h
dn.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/sgtty.h
dn.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
dn.o: /usr/include/sys/ttydev.h
hys.o: hys.c ../condevs.h ../uucp.h /usr/include/stdio.h
hys.o: /usr/include/sysexits.h /usr/include/sys/types.h
hys.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
hys.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
hys.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
hysq.o: hysq.c ../condevs.h ../uucp.h /usr/include/stdio.h
hysq.o: /usr/include/sysexits.h /usr/include/sys/types.h
hysq.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
hysq.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
hysq.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mic.o: mic.c ../condevs.h ../uucp.h /usr/include/stdio.h
mic.o: /usr/include/sysexits.h /usr/include/sys/types.h
mic.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
mic.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
mic.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
nov.o: nov.c ../condevs.h ../uucp.h /usr/include/stdio.h
nov.o: /usr/include/sysexits.h /usr/include/sys/types.h
nov.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
nov.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
nov.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
pen.o: pen.c ../condevs.h ../uucp.h /usr/include/stdio.h
pen.o: /usr/include/sysexits.h /usr/include/sys/types.h
pen.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
pen.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
pen.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
pnet.o: pnet.c ../condevs.h ../uucp.h /usr/include/stdio.h
pnet.o: /usr/include/sysexits.h /usr/include/sys/types.h
pnet.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
pnet.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
pnet.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rvmacs.o: rvmacs.c ../condevs.h ../uucp.h /usr/include/stdio.h
rvmacs.o: /usr/include/sysexits.h /usr/include/sys/types.h
rvmacs.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
rvmacs.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
rvmacs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sy.o: sy.c ../condevs.h ../uucp.h /usr/include/stdio.h /usr/include/sysexits.h
sy.o: /usr/include/sys/types.h /usr/include/sys/timeb.h /usr/include/errno.h
sy.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/sgtty.h
sy.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sy.o: /usr/include/sys/ttydev.h
unet.o: unet.c ../condevs.h ../uucp.h /usr/include/stdio.h
unet.o: /usr/include/sysexits.h /usr/include/sys/types.h
unet.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
unet.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
unet.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
va212.o: va212.c ../condevs.h ../uucp.h /usr/include/stdio.h
va212.o: /usr/include/sysexits.h /usr/include/sys/types.h
va212.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
va212.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
va212.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
va811.o: va811.c ../condevs.h ../uucp.h /usr/include/stdio.h
va811.o: /usr/include/sysexits.h /usr/include/sys/types.h
va811.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
va811.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
va811.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
va820.o: va820.c ../condevs.h ../uucp.h /usr/include/stdio.h
va820.o: /usr/include/sysexits.h /usr/include/sys/types.h
va820.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
va820.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
va820.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vad.o: vad.c ../condevs.h ../uucp.h /usr/include/stdio.h
vad.o: /usr/include/sysexits.h /usr/include/sys/types.h
vad.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
vad.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
vad.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vent.o: vent.c ../condevs.h ../uucp.h /usr/include/stdio.h
vent.o: /usr/include/sysexits.h /usr/include/sys/types.h
vent.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
vent.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
vent.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vmacs.o: vmacs.c ../condevs.h ../uucp.h /usr/include/stdio.h
vmacs.o: /usr/include/sysexits.h /usr/include/sys/types.h
vmacs.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
vmacs.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
vmacs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
hys24.o: hys24.c ../condevs.h ../uucp.h /usr/include/stdio.h
hys24.o: /usr/include/sysexits.h /usr/include/sys/types.h
hys24.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
hys24.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
hys24.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cds224.o: cds224.c ../condevs.h ../uucp.h /usr/include/stdio.h
cds224.o: /usr/include/sysexits.h /usr/include/sys/types.h
cds224.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
cds224.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cds224.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
att2224.o: att2224.c ../condevs.h ../uucp.h /usr/include/stdio.h
att2224.o: /usr/include/sysexits.h /usr/include/sys/types.h
att2224.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
att2224.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
att2224.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
