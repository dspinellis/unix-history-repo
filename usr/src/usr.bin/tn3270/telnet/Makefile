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
#	@(#)Makefile	4.4 (Berkeley) 6/6/90
#

# The following is the telnet makefile for tn3270, using the shared telnet
# sources.

#
# TERMCAP	Define this if your system is termcap based,
#		otherwise a terminfo based system is assumed.
#
# SRCRT		Includes code to allow you to specify source routes.
#		Format is:
#			[!]@hop1@hop2...[@|:]dst
#		Leading ! means strict source route.
#
# NOSTRNCASECMP	Define this if you do not have strncasecmp() in
#		your C libarary.
#
# USE_TERMIO	Define this if you have System V termio structures.
#		What is here is how things are on Cray computers.
#
# KLUDGELINEMODE Define this to get the kludged up version of linemode
#		that was in 4.3BSD.  This is a good thing to have
#		around for talking to older systems.
#

DEFINES= -DTERMCAP -DSRCRT -DKLUDGELINEMODE -DUSE_TERMIO


VPATH	=	${.CURDIR}/../../telnet
XINCLUDES=	-I${.CURDIR}/../../telnet -I${.CURDIR}
INCLUDES=
XDEFINES	=	-DTN3270
OPTIMIZE=	-O
CFLAGS	=	${OPTIMIZE} ${INCLUDES} ${DEFINES}
XCFLAGS=	${XINCLUDES} ${XDEFINES}
LD	=	ld
LDFLAGS	=	-r
PRINT	= print
ACTION = sccs tell
LIBC=	/usr/lib/libc.a
ALLH=	defines.h externs.h fdset.h general.h ring.h types.h
SRCS=	commands.c main.c network.c ring.c \
	sys_bsd.c telnet.c terminal.c \
	tn3270.c utilities.c
ALLHC=		${ALLH} ${SRCS}
ALLPRINT =	${ALLHC}
ALLSOURCE=	${ALLHC} Makefile Makefile_ultrix
OBJS=	commands.o main.o network.o ring.o sys_bsd.o \
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
