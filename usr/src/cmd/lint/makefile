M=/usr/src/cmd/mip
LOAD1=-i -s
LOAD2=-s -n
CFLAGS=-O

all:	head

cp:	head
	cp lpass1 /usr/lib/lint1
	cp lpass2 /usr/lib/lint2
	rm lpass1 lpass2 *.o cgram.c

cmp:	head
	cmp lpass1 /usr/lib/lint1
	cmp lpass2 /usr/lib/lint2
	rm lpass1 lpass2 *.o cgram.c

head: lpass1 lpass2 ;
lpass1: cgram.o xdefs.o scan.o comm1.o pftn.o trees.o optim.o lint.o
	cc $(LOAD1) cgram.o xdefs.o scan.o comm1.o pftn.o trees.o optim.o lint.o
	mv a.out lpass1
trees.o: $M/manifest macdefs $M/mfile1 $M/trees.c
	cc -c $(CFLAGS) -I$M -I. $M/trees.c
optim.o: $M/manifest macdefs $M/mfile1 $M/optim.c
	cc -c $(CFLAGS) -I$M -I. $M/optim.c
pftn.o: $M/manifest macdefs $M/mfile1 $M/pftn.c
	cc -c $(CFLAGS) -I$M -I. $M/pftn.c
lint.o: $M/manifest macdefs $M/mfile1 lmanifest
	cc -c $(CFLAGS) -I$M -I. lint.c
scan.o: $M/manifest macdefs $M/mfile1 $M/scan.c
	cc -c $(CFLAGS) -I$M -I. $M/scan.c
xdefs.o: $M/manifest $M/mfile1 macdefs $M/xdefs.c
	cc -c $(CFLAGS) -I$M -I. $M/xdefs.c
cgram.o: $M/manifest $M/mfile1 macdefs cgram.c
	cc -c $(CFLAGS) -I$M -I. cgram.c
cgram.c: $M/cgram.y
	yacc $M/cgram.y
	mv y.tab.c cgram.c
comm1.o: $M/manifest $M/mfile1 $M/common macdefs $M/comm1.c
	cc -c $(CFLAGS) -I. -I$M $M/comm1.c
shrink:
	rm *.o
clobber: shrink
	rm lpass1 lpass2
lpass2: lpass2.o
	cc -o lpass2 $(LOAD2) lpass2.o
lpass2.o: $M/manifest lmanifest
	cc $(CFLAGS) -c -I$M -I. lpass2.c
lintall:	cgram.c
	lint -hpv -I. -I$M  cgram.c $M/xdefs.c $M/scan.c $M/pftn.c $M/trees.c $M/optim.c lint.c $M/reader.c local2.c order.c $M/match.c $M/allo.c $M/comm1.c table.c
install:
	echo better be newgrp bin...
	diff SHELL /usr/bin/lint
	diff llib-lc /usr/lib
	diff llib-port /usr/lib
	cp lpass1 /usr/lib/lint1
	cp lpass2 /usr/lib/lint2
	cp llib-* /usr/lib
	cp SHELL /usr/bin/lint
	cp SHELL llib-* lmanifest macdefs lint.c lpass2.c /usr/src/cmd/lint
