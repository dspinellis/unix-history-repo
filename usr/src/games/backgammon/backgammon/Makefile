#
# Copyright {c} 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.7	{Berkeley}	%G%
#
LIBC=	/lib/libc.a
LIBS=	-ltermlib
CFLAGS=	-O -DV7 -w
SRCS=	allow.c board.c check.c data.c extra.c fancy.c init.c main.c move.c \
	odds.c one.c save.c subs.c table.c teach.c text.c ttext1.c ttext2.c \
	tutor.c message.c
BSRCS=	allow.c board.c check.c extra.c fancy.c init.c main.c move.c \
	odds.c one.c save.c subs.c table.c text.c message.c
BOBJS=	allow.o board.o check.o extra.o fancy.o init.o main.o move.o \
	odds.o one.o save.o subs.o table.o text.o message.o
TSRCS=	allow.c board.c check.c data.c fancy.c init.c odds.c one.c save.c \
	subs.c table.c teach.c ttext1.c ttext2.c tutor.c
TOBJS=	allow.o board.o check.o data.o fancy.o init.o odds.o one.o save.o \
	subs.o table.o teach.o ttext1.o ttext2.o tutor.o

all: backgammon teachgammon backgammon.doc

# Backgammon program
backgammon: ${BOBJS} ${LIBC}
	${CC} -o $@ ${BOBJS} ${LIBS}

# Backgammon rules and tutorial
teachgammon: ${TOBJS} ${LIBC}
	${CC} -o $@ ${TOBJS} ${LIBS}

# Update message.
message.c: FRC
	-[ ! -w message.c ] || ex - message.c < Mesgfix

# Documentation
backgammon.doc: backgammon.src
	nroff -man -Tcrt backgammon.src > backgammon.doc

clean: FRC
	rm -f ${TOBJS} ${BOBJS} core teachgammon backgammmon backgammon.doc

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 backgammon ${DESTDIR}/usr/games/backgammon
	install -s -o bin -g bin -m 755 teachgammon ${DESTDIR}/usr/games/teachgammon

lint: FRC
	lint ${CFLAGS} ${BRCS}
	lint ${CFLAGS} ${TSRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

allow.o: allow.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
allow.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
board.o: board.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
board.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
check.o: check.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
check.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
data.o: data.c tutor.h
extra.o: extra.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
extra.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
fancy.o: fancy.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
fancy.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
init.o: init.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
init.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: main.c /usr/include/stdio.h back.h /usr/include/sgtty.h
main.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
main.o: /usr/include/sys/ttydev.h
move.o: move.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
move.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
odds.o: odds.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
odds.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
one.o: one.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
one.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
save.o: save.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
save.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
subs.o: subs.c /usr/include/stdio.h back.h /usr/include/sgtty.h
subs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
subs.o: /usr/include/sys/ttydev.h
table.o: table.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
table.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
teach.o: teach.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
teach.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
text.o: text.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
text.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttext1.o: ttext1.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttext1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttext2.o: ttext2.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttext2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tutor.o: tutor.c back.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
tutor.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h tutor.h
message.o: message.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
