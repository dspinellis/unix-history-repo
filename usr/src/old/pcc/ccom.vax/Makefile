#	Makefile	1.7	85/01/18
#
TESTDIR = .
FRC =
O = -O
CFLAGS = ${O} -I. -I${M} -DASSTRINGS -DSTABDOT -DLCOMM -DBUFSTDERR -DFLEXNAMES \
    "-DFIXSTRUCT=outstruct"
LDFLAGS = -g

M=../mip
all: comp scomp fort
comp: rodata.o cgram.o xdefs.o scan.o pftn.o trees.o optim.o code.o local.o \
	reader.o local2.o order.o match.o allo.o comm1.o table.o stab.o
	$(CC) $(LDFLAGS) -z rodata.o cgram.o xdefs.o scan.o pftn.o trees.o \
		optim.o code.o local.o reader.o local2.o order.o match.o \
		allo.o comm1.o table.o stab.o -o $(TESTDIR)/comp
trees.o: $M/manifest macdefs $M/mfile1 $M/trees.c
	$(CC) -c $(CFLAGS) -I$M -I.  $M/trees.c
optim.o: $M/manifest macdefs $M/mfile1 $M/optim.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/optim.c
pftn.o: $M/manifest macdefs $M/mfile1 $M/pftn.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/pftn.c
code.o: $M/manifest macdefs $M/mfile1
	$(CC) -c $(CFLAGS) -I$M -I. code.c
local.o: $M/manifest macdefs $M/mfile1
	$(CC) -c $(CFLAGS) -I$M -I. local.c
scan.o: $M/manifest macdefs $M/mfile1 $M/scan.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/scan.c
xdefs.o: $M/manifest $M/mfile1 macdefs $M/xdefs.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/xdefs.c
cgram.o: $M/manifest $M/mfile1 macdefs cgram.c
	$(CC) -c $(CFLAGS) -I$M -I. cgram.c
rodata.o: rodata.c
	$(CC) $(CFLAGS) -S rodata.c
	sh ./:rofix rodata.s
	$(AS) -o rodata.o rodata.s
	rm -f rodata.s
rodata.c cgram.c: $M/cgram.y
	$(YACC) $M/cgram.y
	rm -f rodata.c
	sh ./:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c
comm1.o: $M/manifest $M/mfile1 $M/common macdefs $M/comm1.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/comm1.c
table.o: $M/manifest $M/mfile2 mac2defs macdefs table.c
	$(CC) $(CFLAGS) -S -I$M -I. table.c
	sh ./:rofix table.s
	$(AS) -o table.o table.s
	rm -f table.s
reader.o: $M/manifest $M/mfile2 mac2defs macdefs $M/reader.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/reader.c
local2.o: $M/manifest $M/mfile2 mac2defs macdefs
	$(CC) -c $(CFLAGS) -I$M -I. local2.c
order.o: $M/manifest $M/mfile2 mac2defs macdefs
	$(CC) -c $(CFLAGS) -I$M -I. order.c
match.o: $M/manifest $M/mfile2 mac2defs macdefs $M/match.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/match.c
allo.o: $M/manifest $M/mfile2 mac2defs macdefs $M/allo.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/allo.c
scomp: rodata.o cgram.o xdefs.o scan.o pftn.o strees.o optim.o code.o slocal.o \
	reader.o slocal2.o sorder.o match.o sallo.o comm1.o stable.o stab.o
	$(CC) $(LDFLAGS) -z rodata.o cgram.o xdefs.o scan.o pftn.o strees.o \
		optim.o code.o slocal.o reader.o slocal2.o sorder.o match.o \
		sallo.o comm1.o stable.o stab.o -o $(TESTDIR)/scomp
strees.o: $M/manifest macdefs $M/mfile1 $M/trees.c
	ln $M/trees.c strees.c
	$(CC) -c $(CFLAGS) -I$M -I. -DSPRECC strees.c
	rm strees.c
slocal.o: $M/manifest $M/mfile2 mac2defs macdefs local.c
	ln local.c slocal.c
	$(CC) -c $(CFLAGS) -I$M -I. -DSPRECC slocal.c
	rm slocal.c
slocal2.o: $M/manifest $M/mfile2 mac2defs macdefs local2.c
	ln local2.c slocal2.c
	$(CC) -c $(CFLAGS) -I$M -I. -DSPRECC slocal2.c
	rm slocal2.c
sallo.o: $M/manifest $M/mfile2 mac2defs macdefs $M/allo.c
	ln $M/allo.c sallo.c
	$(CC) -c $(CFLAGS) -I$M -I. -DSPRECC sallo.c
	rm sallo.c
sorder.o: $M/manifest $M/mfile2 mac2defs macdefs order.c
	ln order.c sorder.c
	$(CC) -c $(CFLAGS) -I$M -I. -DSPRECC sorder.c
	rm sorder.c
stable.o: $M/manifest $M/mfile2 mac2defs macdefs table.c
	$(CC) $(CFLAGS) -S -I$M -I. -DSPRECC table.c
	sh ./:rofix table.s
	$(AS) -o stable.o table.s
	rm -f table.s
fort: comp fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o \
	fcomm2.o
	$(CC) -z $(CFLAGS) fort.o freader.o fallo.o fmatch.o ftable.o \
		forder.o flocal2.o fcomm2.o -o $(TESTDIR)/fort
fort.o: fort.h $M/fort.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/fort.c
freader.o: reader.o
	$(CC) -c $(CFLAGS) -I$M -I. $M/freader.c
fallo.o: allo.o
	$(CC) -c $(CFLAGS) -I$M -I. $M/fallo.c
fmatch.o: match.o
	$(CC) -c $(CFLAGS) -I$M -I. $M/fmatch.c
ftable.o: table.o
	$(CC) -c -R $(CFLAGS) -I$M -I. $M/ftable.c
forder.o: order.o
	$(CC) -c $(CFLAGS) -I$M -I. $M/forder.c
flocal2.o: local2.o
	$(CC) -c $(CFLAGS) -I$M -I. $M/flocal2.c
fcomm2.o: $M/common
	$(CC) -c $(CFLAGS) -I$M -I. $M/fcomm2.c
fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o: \
	$M/mfile2 $M/manifest macdefs mac2defs
install:	all
	install comp ${DESTDIR}/lib/ccom
	install scomp ${DESTDIR}/lib/sccom
	install fort ${DESTDIR}/lib/f1
shrink:
	rm *.o comp scomp fort
clean:
	rm -f *.o comp fort cgram.c rodata.c
lintall:
	lint -hv -I. -I$M  cgram.c $M/xdefs.c $M/scan.c $M/pftn.c \
		$M/trees.c $M/optim.c code.c local.c $M/reader.c \
		local2.c order.c $M/match.c $M/allo.c $M/comm1.c table.c
