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
#	@(#)Makefile	1.13 (Berkeley) %G%
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
	rm -f ${OBJS} core errs l.errs telnet

cleandir: clean
	rm -f ${MAN} tags .depend

clist:	FRC ${SRCS}
	@for i in ${SRCS} ; \
		do (echo ${DIRPATH}$$i); done

hclist:	FRC ${ALLHC}
	@for i in ${ALLHC} ; \
		do (echo ${DIRPATH}$$i); done

depend: FRC ${SRCS}
	mkdep ${CFLAGS} `make clist`

install: FRC
	install -s -o bin -g bin -m 755 telnet ${DESTDIR}/usr/bin

lint: FRC ${SRCS}
	lint ${CFLAGS} `make clist`

tags: FRC ${SRCS}
	ctags `make hclist`

FRC:
