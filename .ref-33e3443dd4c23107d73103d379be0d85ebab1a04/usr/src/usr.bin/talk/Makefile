#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
DESTDIR=
OBJS=	talk.o get_names.o display.o io.o ctl.o init_disp.o \
	msgs.o get_addrs.o ctl_transact.o invite.o look_up.o
SRCS=	talk.c get_names.c display.c io.c ctl.c init_disp.c \
	msgs.c get_addrs.c ctl_transact.c invite.c look_up.c
INCLUDE=talk.h talk_ctl.h
CFLAGS= -O

all:	talk

talk:	${OBJS}
	cc -o talk ${OBJS} -lcurses -ltermlib

${OBJS}: talk.h talk_ctl.h

install: talk
	install -g tty -m 2755 -s talk ${DESTDIR}/usr/ucb/talk

clean:
	rm -f ${OBJS} a.out errs core talk

tags:	${SRCS} ${INCLUDE}
	ctags ${SRCS} ${INCLUDE}

depend: ${SRCS}
	for i in ${SRCS}; do \
	    cc -M $$i | sed 's/\.o//' | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

talk: talk.c ./talk.h /usr/include/curses.h /usr/include/stdio.h
talk: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
talk: /usr/include/sys/ttydev.h /usr/include/utmp.h
get_names: get_names.c ./talk.h /usr/include/curses.h /usr/include/stdio.h
get_names: /usr/include/sgtty.h /usr/include/sys/ioctl.h
get_names: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
get_names: /usr/include/utmp.h /usr/include/sys/param.h
get_names: /usr/include/machine/machparam.h /usr/include/signal.h
get_names: /usr/include/sys/types.h /usr/include/protocols/talkd.h
get_names: /usr/include/sys/types.h /usr/include/sys/socket.h
display: display.c ./talk.h /usr/include/curses.h /usr/include/stdio.h
display: /usr/include/sgtty.h /usr/include/sys/ioctl.h
display: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
display: /usr/include/utmp.h
io: io.c ./talk.h /usr/include/curses.h /usr/include/stdio.h
io: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
io: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/stdio.h
io: /usr/include/errno.h /usr/include/sys/time.h /usr/include/time.h
ctl: ctl.c ./talk_ctl.h /usr/include/sys/types.h /usr/include/protocols/talkd.h
ctl: /usr/include/sys/types.h /usr/include/sys/socket.h
ctl: /usr/include/netinet/in.h ./talk.h /usr/include/curses.h
ctl: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ctl: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h /usr/include/utmp.h
ctl: /usr/include/errno.h
init_disp: init_disp.c ./talk.h /usr/include/curses.h /usr/include/stdio.h
init_disp: /usr/include/sgtty.h /usr/include/sys/ioctl.h
init_disp: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
init_disp: /usr/include/utmp.h /usr/include/signal.h
msgs: msgs.c /usr/include/signal.h /usr/include/stdio.h /usr/include/sys/time.h
msgs: /usr/include/time.h ./talk.h /usr/include/curses.h /usr/include/stdio.h
msgs: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
msgs: /usr/include/sys/ttydev.h /usr/include/utmp.h
get_addrs: get_addrs.c ./talk_ctl.h /usr/include/sys/types.h
get_addrs: /usr/include/protocols/talkd.h /usr/include/sys/types.h
get_addrs: /usr/include/sys/socket.h /usr/include/netinet/in.h ./talk.h
get_addrs: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
get_addrs: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
get_addrs: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h
get_addrs: /usr/include/netdb.h
ctl_transact: ctl_transact.c ./talk_ctl.h /usr/include/sys/types.h
ctl_transact: /usr/include/protocols/talkd.h /usr/include/sys/types.h
ctl_transact: /usr/include/sys/socket.h /usr/include/netinet/in.h ./talk.h
ctl_transact: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
ctl_transact: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ctl_transact: /usr/include/sys/ttydev.h /usr/include/utmp.h
ctl_transact: /usr/include/errno.h /usr/include/sys/time.h /usr/include/time.h
invite: invite.c ./talk_ctl.h /usr/include/sys/types.h
invite: /usr/include/protocols/talkd.h /usr/include/sys/types.h
invite: /usr/include/sys/socket.h /usr/include/netinet/in.h ./talk.h
invite: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
invite: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
invite: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h
invite: /usr/include/sys/time.h /usr/include/time.h /usr/include/signal.h
invite: /usr/include/setjmp.h
look_up: look_up.c ./talk_ctl.h /usr/include/sys/types.h
look_up: /usr/include/protocols/talkd.h /usr/include/sys/types.h
look_up: /usr/include/sys/socket.h /usr/include/netinet/in.h ./talk.h
look_up: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
look_up: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
look_up: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/errno.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
