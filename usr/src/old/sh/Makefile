#	Makefile	4.6	87/05/31
#
CFLAGS = -O -w

all:	sh

cp:	sh
	cp sh /bin/sh
	rm sh *.o

cmp:	sh
	cmp sh /bin/sh
	rm sh *.o

sh:	setbrk.o
sh:	builtin.o blok.o stak.o
sh:	cmd.o fault.o main.o
sh:	word.o string.o name.o args.o
sh:	xec.o service.o error.o io.o
sh:	print.o macro.o expand.o
sh:	ctype.o msg.o
blok.o:		brkincr.h
fault.o:	brkincr.h
main.o:		brkincr.h
stak.o:		brkincr.h

sh:
	cc -o sh *.o

depend:

install:
	install -s sh $(DESTDIR)/bin
clean:
	rm -f sh *.o
