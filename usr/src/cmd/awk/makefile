CFLAGS=-O
YFLAGS=-d

all:	awk

cp:	awk
	cp awk /bin/awk
	rm *.o awk.h proc awk proctab.c y.tab.h

cmp:	awk
	cmp awk /bin/awk
	rm *.o awk.h proc awk proctab.c y.tab.h

FILES=awk.lx.o b.o main.o token.o tran.o lib.o run.o parse.o proctab.o 
SOURCE=awk.def awk.g.y awk.lx.l b.c lib.c main.c parse.c proctab.c \
	proc.c\
	run.c token.c tran.c

awk:	$(FILES) awk.g.o
	cc -i -s $(CFLAGS) awk.g.o $(FILES) -lm -o awk

y.tab.h:	awk.g.o

awk.h:	y.tab.h
	-cmp -s y.tab.h awk.h || cp y.tab.h awk.h

$(FILES):	awk.h awk.def

token.c:	awk.h
	ed - <tokenscript
	rm temp

src:	$(SOURCE) test.a tokenscript makefile
	cp $? /usr/src/cmd/awk
	touch src

profile:	awk.g.o $(FILES)
	cc -p -i awk.g.o $(FILES) -lm

find:
	egrep -n "$(PAT)" *.[ylhc] awk.def

list:
	-pr $(SOURCE) makefile

lint:
	lint -spu b.c main.c token.c tran.c run.c lib.c parse.c -lm |\
		egrep -v '^(error|free|malloc)'

proctab.c:	proc
	proc > proctab.c
proc:	awk.h proc.o token.o
	cc -o proc proc.c token.o
