#
# Copyright (c) 1983, 1988 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
DESTDIR=
DOBJECT=talkd.o announce.o process.o table.o print.o
TOBJECT=talk.o get_names.o display.o io.o ctl.o init_disp.o \
	msgs.o get_addrs.o ctl_transact.o invite.o look_up.o
DSOURCE=talkd.c announce.c process.c table.c print.c
TSOURCE=talk.c get_names.c display.c io.c ctl.c init_disp.c \
	msgs.c get_addrs.c ctl_transact.c invite.c look_up.c
INCLUDE=talk.h ctl.h talk_ctl.h
SOURCE= ${DSOURCE} ${TSOURCE}
CFLAGS= -O 

all: talk talkd

talkd: ${DOBJECT}
	cc ${CFLAGS} -o talkd ${DOBJECT}

talk:	${TOBJECT}
	cc -o talk ${TOBJECT} -lcurses -ltermlib -lcompat

${DOBJECT}: ctl.h
${TOBJECT}: talk.h ctl.h talk_ctl.h

install: talk talkd
	install -g tty -m 2755 -s talk ${DESTDIR}/usr/old/talk
	install -s talkd ${DESTDIR}/usr/libexec/talkd

lint:
	lint ${DSOURCE}

clean:
	rm -f *.o talk talkd

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SOURCE}
	mkdep ${SOURCE}

tags:	${SOURCE} ${INCLUDE}
	ctags ${SOURCE} ${INCLUDE}
