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
#	@(#)Makefile	5.7 (Berkeley) %G%
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

talk.o: talk.c talk.h /usr/include/curses.h /usr/include/stdio.h
talk.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
talk.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
talk.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
get_names.o: get_names.c talk.h /usr/include/curses.h /usr/include/stdio.h
get_names.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
get_names.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
get_names.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
get_names.o: /usr/include/sys/param.h /usr/include/sys/types.h
get_names.o: /usr/include/signal.h /usr/include/machine/trap.h
get_names.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
get_names.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
get_names.o: /usr/include/sys/socket.h /usr/include/pwd.h
display.o: display.c talk.h /usr/include/curses.h /usr/include/stdio.h
display.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
display.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
display.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
io.o: io.c talk.h /usr/include/curses.h /usr/include/stdio.h
io.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
io.o: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
io.o: /usr/include/utmp.h /usr/include/stdio.h /usr/include/errno.h
io.o: /usr/include/sys/time.h /usr/include/time.h
ctl.o: ctl.c talk_ctl.h /usr/include/sys/types.h /usr/include/protocols/talkd.h
ctl.o: /usr/include/sys/types.h /usr/include/sys/socket.h
ctl.o: /usr/include/netinet/in.h talk.h /usr/include/curses.h
ctl.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ctl.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
ctl.o: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h
init_disp.o: init_disp.c talk.h /usr/include/curses.h /usr/include/stdio.h
init_disp.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
init_disp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
init_disp.o: /usr/include/sys/ttydev.h /usr/include/utmp.h
init_disp.o: /usr/include/signal.h /usr/include/machine/trap.h
msgs.o: msgs.c /usr/include/signal.h /usr/include/machine/trap.h
msgs.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h talk.h
msgs.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
msgs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
msgs.o: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
msgs.o: /usr/include/utmp.h
get_addrs.o: get_addrs.c talk_ctl.h /usr/include/sys/types.h
get_addrs.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
get_addrs.o: /usr/include/sys/socket.h /usr/include/netinet/in.h talk.h
get_addrs.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
get_addrs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
get_addrs.o: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
get_addrs.o: /usr/include/utmp.h /usr/include/errno.h /usr/include/netdb.h
ctl_transact.o: ctl_transact.c talk_ctl.h /usr/include/sys/types.h
ctl_transact.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
ctl_transact.o: /usr/include/sys/socket.h /usr/include/netinet/in.h talk.h
ctl_transact.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
ctl_transact.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ctl_transact.o: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
ctl_transact.o: /usr/include/utmp.h /usr/include/errno.h
ctl_transact.o: /usr/include/sys/time.h /usr/include/time.h
invite.o: invite.c talk_ctl.h /usr/include/sys/types.h
invite.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
invite.o: /usr/include/sys/socket.h /usr/include/netinet/in.h talk.h
invite.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
invite.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
invite.o: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
invite.o: /usr/include/utmp.h /usr/include/errno.h /usr/include/sys/time.h
invite.o: /usr/include/time.h /usr/include/signal.h /usr/include/machine/trap.h
invite.o: /usr/include/setjmp.h
look_up.o: look_up.c talk_ctl.h /usr/include/sys/types.h
look_up.o: /usr/include/protocols/talkd.h /usr/include/sys/types.h
look_up.o: /usr/include/sys/socket.h /usr/include/netinet/in.h talk.h
look_up.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
look_up.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
look_up.o: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
look_up.o: /usr/include/utmp.h /usr/include/errno.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
