CFLAGS=-O
YFLAGS=-d

FILES=awk.lx.o b.o main.o token.o tran.o lib.o run.o parse.o proctab.o dump.o 
SOURCE=awk.def awk.g.y awk.lx.l b.c dump.c lib.c main.c parse.c proctab.c \
	proc.c\
	run.c token.c tran.c

awk:	$(FILES) awk.g.o
	cc $(CFLAGS) awk.g.o  $(FILES)  -lm -o awk

y.tab.h:	awk.g.o

awk.h:	y.tab.h
	-cmp -s y.tab.h awk.h || cp y.tab.h awk.h
	rm y.tab.h

$(FILES):	awk.h awk.def

token.c:	awk.h
	ed - <tokenscript

find:
	egrep -n "$(PAT)" *.[ylhc] awk.def

lint:
	lint -spu b.c main.c token.c tran.c run.c lib.c parse.c -lm |\
		egrep -v '^(error|free|malloc)'

proctab.c:	proc
	proc > proctab.c
proc:	awk.h proc.o token.o
	cc -o proc proc.c token.o

install:
	install -s awk $(DESTDIR)/usr/bin
clean:
	rm -f *.o proc proctab.c
