#
# Copyright (c) 1982 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) 5/15/86
#
# mille bourne game makefile
#
HEADERS=mille.h
CFILES=	comp.c end.c extern.c init.c mille.c misc.c move.c print.c \
	roll.c save.c types.c varpush.c
OBJS=	comp.o end.o extern.o init.o mille.o misc.o move.o print.o \
	roll.o save.o types.o varpush.o
POBJS=	comp.po end.po extern.po init.po mille.po misc.po move.po \
	roll.po print.po save.po types.po varpush.po
# CRL=	/jb/ingres/arnold/=lib/=curses/crlib
# CRL=	=curses/screen/libcurses.a
CRL=	
# L=	-ltermlib
L=	-lcurses -ltermlib
LIBS=	${CRL} ${L}
DEFS=
CFLAGS=	-O ${DEFS}
LDFLAGS=
DESTDIR=
BINDIR=	/usr/games
.SUFFIXES: .po .i

.c.po:
	rm -f x.c ; ln $*.c x.c
	${CC} ${CFLAGS} -p -c x.c
	mv x.o $*.po

.c.i:
	${CC} ${LDFLAGS} -P $*.c

a.out: ${OBJS} ${CRL}
	${CC} ${LDFLAGS} ${OBJS} ${LIBS}

mille: ${OBJS} ${CRL}
	${CC} ${CFLAGS} -o mille ${OBJS} ${LIBS}

install: mille
	install -s mille ${DESTDIR}${BINDIR}

pmb: ${POBJS} ../pcrlib
	${CC} ${CFLAGS} -p -o pmb ${POBJS} ../pcrlib -ltermlib

mille.po: mille.c
	rm -f x.c ; ln mille.c x.c
	${CC} ${CFLAGS} -DPROF -p -c x.c
	mv x.o mille.po

table: table.o extern.o
	${CC} ${CFLAGS} -i -o table table.o extern.o

readdump: readdump.o extern.o varpush.o
	${CC} ${CFLAGS} -i -o readdump readdump.o extern.o varpush.o

ctags:
	ctags ${HEADERS} ${CFILES}
	ed - tags < :ctfix
	sort tags -o tags

lint:
	lint -hxb ${DEFS} ${CFILES} ${L} > lint.out

mille.ar:
	ar ruv mille.ar Makefile tags ${HEADERS} ${CFILES}

tar:
	tar rvf /dev/rmt0 Makefile tags :ctfix ${HEADERS} ${CFILES}

lpr:
	pr Makefile ${HEADERS} ${CFILES} tags | lpr ; lpq

clean:
	rm -f ${OBJS} ${POBJS} core ? a.out errs mille lint.out mille.ar \
		tags pmb

depend:
