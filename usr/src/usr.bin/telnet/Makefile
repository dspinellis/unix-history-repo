#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	1.10 (Berkeley) %G%
#

# The following is the telnet makefile.  The sources are down one level
# in directory "Source" because the sources are shared between telnet
# and tn3270.

VPATH	=	Source
INCLUDES=	-ISource
CFLAGS=	-O ${INCLUDES}
LIBC=	/lib/libc.a
SRCS=	commands.c main.c network.c ring.c \
	sys_bsd.c sys_dos.c telnet.c terminal.c \
	tn3270.c utilities.c

ALLHC=	${SRCS} \
	Source/defines.h Source/externs.h Source/fdset.h Source/general.h \
	Source/ring.h Source/types.h

OBJS=	commands.o main.o network.o ring.o sys_bsd.o sys_dos.o \
	telnet.o terminal.o tn3270.o utilities.o

all: telnet

telnet:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core errs l.errs tags telnet

clist:	FRC ${SRCS}
	@for i in ${SRCS} ; \
		do (echo ${DIRPATH}$$i); done

hclist:	FRC ${ALLHC}
	@for i in ${ALLHC} ; \
		do (echo ${DIRPATH}$$i); done

depend: FRC ${SRCS}
	mkdep ${CFLAGS} `make clist`

install: FRC
	install -s -o bin -g bin -m 755 telnet ${DESTDIR}/usr/ucb/telnet

lint: FRC ${SRCS}
	lint ${CFLAGS} `make clist`

tags: FRC ${SRCS}
	ctags `make hclist`

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

commands.o: Source/commands.c /usr/include/sys/types.h
commands.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
commands.o: /usr/include/signal.h /usr/include/machine/trap.h
commands.o: /usr/include/netdb.h /usr/include/ctype.h
commands.o: /usr/include/arpa/telnet.h Source/ring.h Source/externs.h
commands.o: /usr/include/stdio.h /usr/include/setjmp.h Source/defines.h
commands.o: Source/types.h
main.o: Source/main.c /usr/include/sys/types.h Source/ring.h Source/externs.h
main.o: /usr/include/stdio.h /usr/include/setjmp.h Source/defines.h
network.o: Source/network.c /usr/include/sys/types.h /usr/include/sys/socket.h
network.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
network.o: /usr/include/arpa/telnet.h Source/ring.h Source/defines.h
network.o: Source/externs.h /usr/include/stdio.h /usr/include/setjmp.h
network.o: Source/fdset.h
ring.o: Source/ring.c /usr/include/stdio.h /usr/include/errno.h
ring.o: /usr/include/sys/types.h /usr/include/sys/ioctl.h
ring.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ring.o: /usr/include/sys/socket.h Source/ring.h Source/general.h
sys_bsd.o: Source/sys_bsd.c /usr/include/sys/ioctl.h
sys_bsd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sys_bsd.o: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
sys_bsd.o: /usr/include/sys/socket.h /usr/include/signal.h
sys_bsd.o: /usr/include/machine/trap.h /usr/include/errno.h Source/ring.h
sys_bsd.o: Source/fdset.h Source/defines.h Source/externs.h
sys_bsd.o: /usr/include/stdio.h /usr/include/setjmp.h Source/types.h
sys_dos.o: Source/sys_dos.c
telnet.o: Source/telnet.c /usr/include/sys/types.h /usr/include/curses.h
telnet.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
telnet.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
telnet.o: /usr/include/arpa/telnet.h /usr/include/strings.h Source/ring.h
telnet.o: Source/defines.h Source/externs.h /usr/include/stdio.h
telnet.o: /usr/include/setjmp.h Source/types.h Source/general.h
telnet.o: /usr/include/varargs.h
terminal.o: Source/terminal.c /usr/include/arpa/telnet.h
terminal.o: /usr/include/sys/types.h Source/ring.h Source/externs.h
terminal.o: /usr/include/stdio.h /usr/include/setjmp.h Source/types.h
tn3270.o: Source/tn3270.c Source/fdset.h
utilities.o: Source/utilities.c /usr/include/arpa/telnet.h
utilities.o: /usr/include/sys/types.h /usr/include/ctype.h Source/ring.h
utilities.o: Source/externs.h /usr/include/stdio.h /usr/include/setjmp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
