M=/usr/src/cmd/mip
LOAD1=
LOAD2=
CFLAGS=-O

all:	head

install: head
	install -s lpass1 $(DESTDIR)/usr/lib/lint1
	install -s lpass2 $(DESTDIR)/usr/lib/lint2
	install -c llib-lc $(DESTDIR)/usr/lib
	install -c llib-port $(DESTDIR)/usr/lib
	install -c SHELL $(DESTDIR)/usr/bin/lint

cmp:	head
	cmp lpass1 /usr/lib/lint1
	cmp lpass2 /usr/lib/lint2
	rm lpass1 lpass2 *.o cgram.c

clean : 
	rm -f *.o lpass1 lpass2 cgram.c

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
	cc $(LOAD2) lpass2.o
	mv a.out lpass2
lpass2.o: $M/manifest lmanifest
	cc $(CFLAGS) -c -I$M -I. lpass2.c
