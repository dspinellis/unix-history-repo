#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
DESTDIR=
CFLAGS= -O -DV7
OBJS= allow.o board.o check.o extra.o fancy.o init.o main.o move.o\
	odds.o one.o save.o subs.o table.o text.o message.o
TOBJS= allow.o board.o check.o data.o fancy.o init.o odds.o one.o save.o subs.o\
	table.o teach.o ttext1.o ttext2.o tutor.o
SRCS= allow.c board.c check.c data.c extra.c fancy.c init.c main.c\
	move.c odds.c one.c save.c subs.c table.c teach.c text.c ttext1.c\
	ttext2.c tutor.c message.c

all: backgammon teachgammon backgammon.doc

#	Backgammon program
backgammon: $(OBJS)
	-rm -f backgammon
	cc -o backgammon $(OBJS) -ltermlib

#	Backgammon rules and tutorial
teachgammon: ${TOBJS}
	-rm -f teachgammon
	cc -o teachgammon $(TOBJS) -ltermlib

#	Header files back.h and tutor.h
allow.o board.o check.o extra.o fancy.o main.o move.o odds.o one.o save.o\
	subs.o table.o teach.o text.o ttext1.o ttext2.o: back.h
data.o tutor.o: back.h tutor.h

#	Update message.  /tmp is always changing, so use it to ensure execution
message.c: /tmp
	ex - message.c < Mesgfix

#	Documentation
backgammon.doc: backgammon.src
	-rm -f backgammon.doc
	nroff -man -Tcrt backgammon.src > backgammon.doc

#	Installation
install:
	install backgammon ${DESTDIR}/usr/games/backgammon
	install teachgammon ${DESTDIR}/usr/games/teachgammon

clean:
	rm -f ${OBJS} ${TOBJS}
