INSTALL=/usr/lib/ccom
M=/usr/src/cmd/mip
CFLAGS=-O
head:  ccom ;

all:	ccom

cp:	ccom
	cp ccom /usr/lib/ccom
	rm *.o cgram.c ccom

cmp:	ccom
	cmp ccom /usr/lib/ccom
	rm *.o cgram.c ccom

ccom: cgram.o xdefs.o scan.o pftn.o trees.o optim.o code.o local.o reader.o local2.o order.o match.o allo.o comm1.o table.o
	cc $(CFLAGS) -s -o ccom -i cgram.o xdefs.o scan.o pftn.o trees.o optim.o code.o local.o reader.o local2.o order.o match.o allo.o comm1.o table.o
trees.o: $M/manifest macdefs $M/mfile1 $M/trees.c
	cc -c $(CFLAGS) -I$M -I. $M/trees.c
optim.o: $M/manifest macdefs $M/mfile1 $M/optim.c
	cc -c $(CFLAGS) -I$M -I. $M/optim.c
pftn.o: $M/manifest macdefs $M/mfile1 $M/pftn.c
	cc -c $(CFLAGS) -I$M -I. $M/pftn.c
code.o: $M/manifest macdefs $M/mfile1
	cc -c $(CFLAGS) -I$M -I. code.c
local.o: $M/manifest macdefs $M/mfile1
	cc -c $(CFLAGS) -I$M -I. local.c
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
table.o: $M/manifest $M/mfile2 mac2defs macdefs table.c
	cc -c $(CFLAGS) -I$M -I. table.c
reader.o: $M/manifest $M/mfile2 mac2defs macdefs $M/reader.c
	cc -c $(CFLAGS) -I$M -I. $M/reader.c
local2.o: $M/manifest $M/mfile2 mac2defs macdefs
	cc -c $(CFLAGS) -I$M -I. local2.c
order.o: $M/manifest $M/mfile2 mac2defs macdefs
	cc -c $(CFLAGS) -I$M -I. order.c
match.o: $M/manifest $M/mfile2 mac2defs macdefs $M/match.c
	cc -c $(CFLAGS) -I$M -I. $M/match.c
allo.o: $M/manifest $M/mfile2 mac2defs macdefs $M/allo.c
	cc -c $(CFLAGS) -I$M -I. $M/allo.c
shrink:
	rm *.o ccom
lintall:
	lint -hpv -I. -I$M  cgram.c xdefs.c $M/scan.c $M/pftn.c $M/trees.c $M/optim.c code.c local.c $M/reader.c local2.c order.c $M/match.c $M/allo.c $M/comm1.c table.c
fort: ccom fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o
	cc -i $(CFLAGS) fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o
	mv a.out fort
fort.o: fort.h $M/fort.c
	cc -c $(CFLAGS) -I$M -I. $M/fort.c
freader.o: reader.o
	cc -c $(CFLAGS) -I$M -I. $M/freader.c
fallo.o: allo.o
	cc -c $(CFLAGS) -I$M -I. $M/fallo.c
fmatch.o: match.o
	cc -c $(CFLAGS) -I$M -I. $M/fmatch.c
ftable.o: table.o
	cc -c $(CFLAGS) -I$M -I. $M/ftable.c
forder.o: order.o
	cc -c $(CFLAGS) -I$M -I. $M/forder.c
flocal2.o: local2.o
	cc -c $(CFLAGS) -I$M -I. $M/flocal2.c
fcomm2.o: $M/common
	cc -c $(CFLAGS) -I$M -I. $M/fcomm2.c
fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o: $M/mfile2 $M/manifest macdefs mac2defs
install:
	cp ccom $(INSTALL)
	cp macdefs mac2defs local.c code.c local2.c order.c table.c /usr/src/cmd/pcc
