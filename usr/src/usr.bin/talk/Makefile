#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	talk.c get_names.c display.c io.c ctl.c init_disp.c \
	msgs.c get_addrs.c ctl_transact.c invite.c look_up.c
OBJS=	talk.o get_names.o display.o io.o ctl.o init_disp.o \
	msgs.o get_addrs.o ctl_transact.o invite.o look_up.o

all: talk

talk:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -lcurses -ltermlib

clean: FRC
	rm -f ${OBJS} core talk

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g tty -m 2755 talk ${DESTDIR}/usr/ucb/talk

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

talk.o: talk.c
talk.o:talk.h
talk.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
talk.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
talk.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
get_names.o: get_names.c
get_names.o:talk.h
get_names.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
get_names.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
get_names.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
get_names.o: /usr/include/sys/param.h /usr/include/sys/types.h
get_names.o: /usr/include/signal.h /usr/include/machine/machparam.h
get_names.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
get_names.o: /usr/include/sys/socket.h
display.o: display.c
display.o:talk.h
display.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
display.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
display.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
io.o: io.c
io.o:talk.h
io.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
io.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
io.o: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/stdio.h
io.o: /usr/include/errno.h /usr/include/sys/time.h /usr/include/time.h
ctl.o: ctl.c
ctl.o:talk_ctl.h
ctl.o: /usr/include/sys/types.h /usr/include/protocols/talkd.h
ctl.o: /usr/include/sys/types.h /usr/include/sys/socket.h
ctl.o: /usr/include/netinet/in.h
ctl.o:talk.h
ctl.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
ctl.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ctl.o: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h
init_disp.o: init_disp.c
init_disp.o:talk.h
init_disp.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
init_disp.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
init_disp.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
init_disp.o: /usr/include/signal.h
msgs.o: msgs.c /usr/include/signal.h /usr/include/stdio.h
msgs.o: /usr/include/sys/time.h /usr/include/time.h
msgs.o:talk.h
msgs.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
msgs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
msgs.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
get_addrs.o: get_addrs.c
get_addrs.o:talk_ctl.h
get_addrs.o: /usr/include/sys/types.h /usr/include/protocols/talkd.h
get_addrs.o: /usr/include/sys/types.h /usr/include/sys/socket.h
get_addrs.o: /usr/include/netinet/in.h
get_addrs.o:talk.h
get_addrs.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
get_addrs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
get_addrs.o: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h
get_addrs.o: /usr/include/netdb.h
ctl_transact.o: ctl_transact.c
ctl_transact.o:talk_ctl.h
ctl_transact.o: /usr/include/sys/types.h /usr/include/protocols/talkd.h
ctl_transact.o: /usr/include/sys/types.h /usr/include/sys/socket.h
ctl_transact.o: /usr/include/netinet/in.h
ctl_transact.o:talk.h
ctl_transact.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
ctl_transact.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ctl_transact.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
ctl_transact.o: /usr/include/errno.h /usr/include/sys/time.h
ctl_transact.o: /usr/include/time.h
invite.o: invite.c
invite.o:talk_ctl.h
invite.o: /usr/include/sys/types.h /usr/include/protocols/talkd.h
invite.o: /usr/include/sys/types.h /usr/include/sys/socket.h
invite.o: /usr/include/netinet/in.h
invite.o:talk.h
invite.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
invite.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
invite.o: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h
invite.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/signal.h
invite.o: /usr/include/setjmp.h
look_up.o: look_up.c
look_up.o:talk_ctl.h
look_up.o: /usr/include/sys/types.h /usr/include/protocols/talkd.h
look_up.o: /usr/include/sys/types.h /usr/include/sys/socket.h
look_up.o: /usr/include/netinet/in.h
look_up.o:talk.h
look_up.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
look_up.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
look_up.o: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
