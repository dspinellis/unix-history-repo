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
#	@(#)Makefile	5.7 (Berkeley) 6/18/88
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
SRCS=	main.c pl_main.c pl_1.c pl_2.c pl_3.c pl_4.c pl_5.c pl_6.c pl_7.c \
	dr_main.c dr_1.c dr_2.c dr_3.c dr_4.c dr_5.c lo_main.c \
	assorted.c game.c globals.c misc.c parties.c sync.c version.c
OBJS=	main.o pl_main.o pl_1.o pl_2.o pl_3.o pl_4.o pl_5.o pl_6.o pl_7.o \
	dr_main.o dr_1.o dr_2.o dr_3.o dr_4.o dr_5.o lo_main.o \
	assorted.o game.o globals.o misc.o parties.o sync.o version.o

all: sail

sail: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core sail

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -c -o games -g bin -m 600 /dev/null ${DESTDIR}/usr/games/lib/saillog
	install -s -o games -g bin -m 4700 sail ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f sail; ln -s dm sail; chown games.bin sail)

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c externs.h /usr/include/stdio.h /usr/include/signal.h
main.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
main.o: machdep.h
pl_main.o: pl_main.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_main.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_main.o: /usr/include/stdio.h /usr/include/signal.h
pl_main.o: /usr/include/machine/trap.h /usr/include/ctype.h
pl_main.o: /usr/include/setjmp.h machdep.h /usr/include/sys/types.h
pl_main.o: /usr/include/sys/wait.h /usr/include/machine/endian.h
pl_1.o: pl_1.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_1.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_1.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
pl_1.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
pl_1.o: /usr/include/sys/types.h /usr/include/sys/wait.h
pl_1.o: /usr/include/machine/endian.h
pl_2.o: pl_2.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_2.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_2.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
pl_2.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
pl_3.o: pl_3.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_3.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_3.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_3.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
pl_3.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
pl_4.o: pl_4.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_4.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_4.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_4.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
pl_4.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
pl_5.o: pl_5.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_5.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_5.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_5.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
pl_5.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
pl_6.o: pl_6.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_6.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_6.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_6.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
pl_6.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
pl_7.o: pl_7.c player.h /usr/include/curses.h /usr/include/stdio.h
pl_7.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pl_7.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h externs.h
pl_7.o: /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
pl_7.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
dr_main.o: dr_main.c driver.h externs.h /usr/include/stdio.h
dr_main.o: /usr/include/signal.h /usr/include/machine/trap.h
dr_main.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
dr_1.o: dr_1.c driver.h externs.h /usr/include/stdio.h /usr/include/signal.h
dr_1.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
dr_1.o: machdep.h
dr_2.o: dr_2.c driver.h externs.h /usr/include/stdio.h /usr/include/signal.h
dr_2.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
dr_2.o: machdep.h
dr_3.o: dr_3.c driver.h externs.h /usr/include/stdio.h /usr/include/signal.h
dr_3.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
dr_3.o: machdep.h
dr_4.o: dr_4.c externs.h /usr/include/stdio.h /usr/include/signal.h
dr_4.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
dr_4.o: machdep.h
dr_5.o: dr_5.c externs.h /usr/include/stdio.h /usr/include/signal.h
dr_5.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
dr_5.o: machdep.h
lo_main.o: lo_main.c /usr/include/pwd.h externs.h /usr/include/stdio.h
lo_main.o: /usr/include/signal.h /usr/include/machine/trap.h
lo_main.o: /usr/include/ctype.h /usr/include/setjmp.h machdep.h
assorted.o: assorted.c externs.h /usr/include/stdio.h /usr/include/signal.h
assorted.o: /usr/include/machine/trap.h /usr/include/ctype.h
assorted.o: /usr/include/setjmp.h machdep.h
game.o: game.c externs.h /usr/include/stdio.h /usr/include/signal.h
game.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
game.o: machdep.h
globals.o: globals.c externs.h /usr/include/stdio.h /usr/include/signal.h
globals.o: /usr/include/machine/trap.h /usr/include/ctype.h
globals.o: /usr/include/setjmp.h machdep.h
misc.o: misc.c externs.h /usr/include/stdio.h /usr/include/signal.h
misc.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
misc.o: machdep.h /usr/include/sys/file.h
parties.o: parties.c externs.h /usr/include/stdio.h /usr/include/signal.h
parties.o: /usr/include/machine/trap.h /usr/include/ctype.h
parties.o: /usr/include/setjmp.h machdep.h
sync.o: sync.c externs.h /usr/include/stdio.h /usr/include/signal.h
sync.o: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/setjmp.h
sync.o: machdep.h /usr/include/sys/file.h /usr/include/sys/errno.h
sync.o: /usr/include/sys/types.h /usr/include/sys/stat.h
version.o: version.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
