#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	1.6 (Berkeley) %G%
#

# The following is the telnet makefile for tn3270, using the shared telnet
# sources.

VPATH	=	../../telnet/Source
XINCLUDES=	-I../../telnet/Source
INCLUDES=	-I.
XDEFINES	=	-DTN3270
OPTIMIZE=	-O
CFLAGS	=	${OPTIMIZE} ${INCLUDES} ${DEFINES}
XCFLAGS=	${XINCLUDES} ${XDEFINES}
LD	=	ld
LDFLAGS	=	-r
PRINT	= print
ACTION = sccs tell
LIBC=	/lib/libc.a
ALLH=	defines.h externs.h fdset.h general.h ring.h types.h
SRCS=	commands.c main.c network.c ring.c \
	sys_bsd.c sys_dos.c telnet.c terminal.c \
	tn3270.c utilities.c
ALLHC=		${ALLH} ${SRCS}
ALLPRINT =	${ALLHC}
ALLSOURCE=	${ALLHC} Makefile Makefile_ultrix
OBJS=	commands.o main.o network.o ring.o sys_bsd.o sys_dos.o \
	telnet.o terminal.o tn3270.o utilities.o

.c.o:
	${CC} -c ${CFLAGS} ${XCFLAGS} $<

telprog.o:	${OBJS} ${LIBC}
	${LD} ${LDFLAGS} -o $@ ${OBJS}

clean: FRC
	rm -f telprog.o ${OBJS} core telnet

depend: FRC ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

lint: FRC ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: FRC ${ALLHC}
	ctags ${ALLHC}

print:	FRC ${ALLPRINT}
	${PRINT} ${ALLPRINT}

action: FRC
	${ACTION}

clist:	FRC ${SRCS}
	@for i in ${SRCS} ; \
		do (echo ${DIRPATH}$$i); done

hclist:	FRC ${ALLHC}
	@for i in ${ALLHC} ; \
		do (echo ${DIRPATH}$$i); done

sourcelist:	FRC ${ALLSOURCE}
	@for i in ${ALLSOURCE} ../../telnet/Makefile ; \
		do (echo ${DIRPATH}$$i); done

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

commands.o: ../../telnet/Source/commands.c /usr/include/sys/types.h
commands.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
commands.o: /usr/include/signal.h /usr/include/machine/trap.h
commands.o: /usr/include/netdb.h /usr/include/ctype.h
commands.o: /usr/include/arpa/telnet.h ../../telnet/Source/ring.h
commands.o: ../../telnet/Source/externs.h /usr/include/stdio.h
commands.o: /usr/include/setjmp.h ../../telnet/Source/defines.h
commands.o: ../../telnet/Source/types.h
main.o: ../../telnet/Source/main.c /usr/include/sys/types.h
main.o: ../../telnet/Source/ring.h ../../telnet/Source/externs.h
main.o: /usr/include/stdio.h /usr/include/setjmp.h
main.o: ../../telnet/Source/defines.h
network.o: ../../telnet/Source/network.c /usr/include/sys/types.h
network.o: /usr/include/sys/socket.h /usr/include/sys/time.h
network.o: /usr/include/time.h /usr/include/errno.h /usr/include/arpa/telnet.h
network.o: ../../telnet/Source/ring.h ../../telnet/Source/defines.h
network.o: ../../telnet/Source/externs.h /usr/include/stdio.h
network.o: /usr/include/setjmp.h ../../telnet/Source/fdset.h
ring.o: ../../telnet/Source/ring.c /usr/include/stdio.h /usr/include/errno.h
ring.o: /usr/include/sys/types.h /usr/include/sys/ioctl.h
ring.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ring.o: /usr/include/sys/socket.h ../../telnet/Source/ring.h
ring.o: ../../telnet/Source/general.h
sys_bsd.o: ../../telnet/Source/sys_bsd.c /usr/include/sys/ioctl.h
sys_bsd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sys_bsd.o: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
sys_bsd.o: /usr/include/sys/socket.h /usr/include/signal.h
sys_bsd.o: /usr/include/machine/trap.h /usr/include/errno.h
sys_bsd.o: ../../telnet/Source/ring.h ../../telnet/Source/fdset.h
sys_bsd.o: ../../telnet/Source/defines.h ../../telnet/Source/externs.h
sys_bsd.o: /usr/include/stdio.h /usr/include/setjmp.h
sys_bsd.o: ../../telnet/Source/types.h
sys_dos.o: ../../telnet/Source/sys_dos.c
telnet.o: ../../telnet/Source/telnet.c /usr/include/sys/types.h
telnet.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
telnet.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
telnet.o: /usr/include/sys/ttydev.h /usr/include/arpa/telnet.h
telnet.o: /usr/include/strings.h ../../telnet/Source/ring.h
telnet.o: ../../telnet/Source/defines.h ../../telnet/Source/externs.h
telnet.o: /usr/include/stdio.h /usr/include/setjmp.h
telnet.o: ../../telnet/Source/types.h ../../telnet/Source/general.h
telnet.o: /usr/include/varargs.h
terminal.o: ../../telnet/Source/terminal.c /usr/include/arpa/telnet.h
terminal.o: /usr/include/sys/types.h ../../telnet/Source/ring.h
terminal.o: ../../telnet/Source/externs.h /usr/include/stdio.h
terminal.o: /usr/include/setjmp.h ../../telnet/Source/types.h
tn3270.o: ../../telnet/Source/tn3270.c ../../telnet/Source/fdset.h
utilities.o: ../../telnet/Source/utilities.c /usr/include/arpa/telnet.h
utilities.o: /usr/include/sys/types.h /usr/include/ctype.h
utilities.o: ../../telnet/Source/ring.h ../../telnet/Source/externs.h
utilities.o: /usr/include/stdio.h /usr/include/setjmp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
