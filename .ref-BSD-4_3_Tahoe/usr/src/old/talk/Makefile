#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) 6/6/85
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

all:	talk talkd

talkd: ${DOBJECT}
	cc ${CFLAGS} -o talkd ${DOBJECT}

talk:	${TOBJECT}
	cc -o talk ${TOBJECT} -lcurses -ltermlib

${DOBJECT}: ctl.h
${TOBJECT}: talk.h ctl.h talk_ctl.h

install: talk talkd
	install -g tty -m 2755 -s talk ${DESTDIR}/usr/old/talk
	install -s talkd ${DESTDIR}/etc/talkd

lint:
	lint ${DSOURCE}

clean:
	rm -f *.o talk talkd

tags:	${SOURCE} ${INCLUDE}
	ctags ${SOURCE} ${INCLUDE}
