HEADERS=mille.h
CFILES=	comp.c end.c extern.c init.c mille.c misc.c move.c print.c \
	roll.c save.c types.c varpush.c
OBJS=	comp.o end.o extern.o init.o mille.o misc.o move.o print.o \
	roll.o save.o types.o varpush.o
POBJS=	comp.po end.po extern.po init.po mille.po misc.po move.po \
	roll.po print.po save.po types.po varpush.po
# CRL=	-lcurses
CRL=	../crlib
LIBS=	${CRL} -ltermcap
CFLAGS=	-O -DSTANDOUT
.SUFFIXES: .po .i

.c.po:
	rm -f x.c ; ln $*.c x.c
	${CC} ${CFLAGS} -p -c x.c
	mv x.o $*.po

.c.i:
	${CC} ${CFLAGS} -P $*.c

a.out: ${OBJS} ${CRL}
	${CC} ${CFLAGS} -n ${OBJS} ${LIBS}

mille: ${OBJS} ${CRL}
	${CC} ${CFLAGS} -n -o mille ${OBJS} ${LIBS}

install: mille
	cp mille mb
	strip mille
	chmod 711 mille
	rm -f /usr/tmp/mb
	ln /usr/public/mille /usr/tmp/mb
	pubrm -f /usr/public/mille
	public mille
	mv mb mille
	rm -f /usr/tmp/mb

pmb: ${POBJS} ../pcrlib
	${CC} ${CFLAGS} -n -p -o pmb ${POBJS} ../pcrlib -ltermlib

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
	lint -hxb ${CFILES} > lint.out

mille.ar:
	ar ruv mille.ar Makefile tags ${HEADERS} ${CFILES}

tar:
	tar crvf mille.tar Makefile tags :ctfix ${HEADERS} ${CFILES}

lpr:
	pr Makefile ${HEADERS} ${CFILES} tags | lpr ; lpq
