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
#	@(#)Makefile	1.18 (Berkeley) %G%
#

# The following is the telnet makefile.  The sources are down one level
# in directory "Source" because the sources are shared between telnet
# and tn3270.

VPATH	=	Source
INCLUDES=	-ISource

#
# TERMCAP	Define this if your system is termcap based,
#		otherwise a terminfo based system is assumed.
#
# SRCRT		Includes code to allow you to specify source routes.
#		Format is:
#			[!]@hop1@hop2...[@|:]dst
#		Leading ! means strict source route.
#
# USE_TERMIO	Define this if you have System V termio structures.
#		What is here is how things are on Cray computers.
#
# KLUDGELINEMODE Define this to get the kludged up version of linemode
#		that was in 4.3BSD.  This is a good thing to have
#		around for talking to older systems.
#
# NOSTRNCASECMP	Define this if you do not have strncasecmp() in
#		your C libarary.
#
# strrchr=rindex Define this if you do not have strrchr() in your
#		C libaray (but you do have rindex...)
#

DEFINES= -DTERMCAP -DSRCRT -DKLUDGELINEMODE -DUSE_TERMIO

#
# LIBS should be set to any libraries that need to be included,
# like the termcap or terminfo library.  LIBPATH is the paths
# to these libraries, for dependencies.
#	For CRAY: LIBS= -lcurses -lnet
#
LIBS=	-ltermcap
LIBPATH= /lib/libc.a /usr/lib/libtermcap.a

SRCS=	commands.c main.c network.c ring.c \
	sys_bsd.c telnet.c terminal.c \
	tn3270.c utilities.c

CFLAGS=	-O ${INCLUDES} ${DEFINES}

ALLHC=	${SRCS} \
	Source/defines.h Source/externs.h Source/fdset.h Source/general.h \
	Source/ring.h Source/types.h

OBJS=	commands.o main.o network.o ring.o sys_bsd.o \
	telnet.o terminal.o tn3270.o utilities.o
MAN=	telnet.0

#
# 4.4BSD
#		DEST=${DESTDIR}/usr/bin
#
# 4.3BSD
#		DEST=${DESTDIR}/usr/ucb
#
DEST=${DESTDIR}/usr/bin

#
# These next three lines are not needed in 4.4BSD
#
# .SUFFIXES: .0 .1
# .1.0:
# 	nroff -man -h $< > $@

all: telnet

telnet:	${OBJS} ${LIBPATH}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

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

install: ${MAN} FRC
	install -s -o bin -g bin -m 755 telnet ${DEST}
	install -c -o bin -g bin -m 444 telnet.0 ${DESTDIR}/usr/man/cat1

lint: FRC ${SRCS}
	lint ${CFLAGS} `make clist`

tags: FRC ${SRCS}
	ctags `make hclist`

FRC:
