#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.6	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	commands.c main.c network.c ring.c sys_bsd.c sys_dos.c \
	telnet.c terminal.c tn3270.c utilities.c
OBJS=	commands.o main.o network.o ring.o sys_bsd.o sys_dos.o \
	telnet.o terminal.o tn3270.o utilities.o

all: telnet

telnet:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core telnet

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 telnet ${DESTDIR}/usr/ucb/telnet

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

commands.o: commands.c /usr/include/sys/types.h /usr/include/sys/socket.h
commands.o: /usr/include/netinet/in.h /usr/include/signal.h
commands.o: /usr/include/machine/trap.h /usr/include/netdb.h
commands.o: /usr/include/ctype.h /usr/include/arpa/telnet.h externs.h
commands.o: /usr/include/stdio.h /usr/include/setjmp.h defines.h types.h
main.o: main.c externs.h /usr/include/stdio.h /usr/include/setjmp.h defines.h
network.o: network.c /usr/include/sys/types.h /usr/include/sys/socket.h
network.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
network.o: /usr/include/arpa/telnet.h defines.h externs.h /usr/include/stdio.h
network.o: /usr/include/setjmp.h
sys_bsd.o: sys_bsd.c /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sys_bsd.o: /usr/include/sys/ttydev.h /usr/include/sys/time.h
sys_bsd.o: /usr/include/time.h /usr/include/signal.h
sys_bsd.o: /usr/include/machine/trap.h defines.h externs.h /usr/include/stdio.h
sys_bsd.o: /usr/include/setjmp.h types.h
sys_dos.o: sys_dos.c
telnet.o: telnet.c /usr/include/sys/types.h /usr/include/sys/time.h
telnet.o: /usr/include/time.h /usr/include/sys/socket.h
telnet.o: /usr/include/netinet/in.h /usr/include/curses.h /usr/include/stdio.h
telnet.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
telnet.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
telnet.o: /usr/include/arpa/telnet.h /usr/include/arpa/inet.h
telnet.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/netdb.h
telnet.o: /usr/include/strings.h defines.h externs.h /usr/include/stdio.h
telnet.o: /usr/include/setjmp.h types.h general.h
terminal.o: terminal.c /usr/include/arpa/telnet.h externs.h
terminal.o: /usr/include/stdio.h /usr/include/setjmp.h types.h
tn3270.o: tn3270.c
utilities.o: utilities.c /usr/include/arpa/telnet.h /usr/include/ctype.h
utilities.o: externs.h /usr/include/stdio.h /usr/include/setjmp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
