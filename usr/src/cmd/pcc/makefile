INSTALL=ccom
FINSTALL=f1
M=/usr/src/cmd/mip
CFLAGS=-O 
head:  comp ;
comp: rodata.o cgram.o xdefs.o scan.o pftn.o trees.o optim.o code.o local.o reader.o local2.o order.o match.o allo.o comm1.o table.o
	cc $(CFLAGS) -i rodata.o cgram.o xdefs.o scan.o pftn.o trees.o optim.o code.o local.o  reader.o local2.o order.o match.o allo.o comm1.o table.o
	mv a.out comp
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
cgram.o: $M/manifest $M/mfile1 macdefs $M/cgram.c
	cc -c $(CFLAGS) -I$M -I. $M/cgram.c
rodata.o $M/cgram.c: $M/cgram.y
	yacc $M/cgram.y
	rm -f rodata.c
	:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c $M/cgram.c
	cc $(CFLAGS) -S rodata.c
	:rofix rodata.s
	as -o rodata.o rodata.s
	rm -f rodata.s rodata.c
comm1.o: $M/manifest $M/mfile1 $M/common macdefs $M/comm1.c
	cc -c $(CFLAGS) -I. -I$M $M/comm1.c
table.o: $M/manifest $M/mfile2 mac2defs macdefs table.c
	cc $(CFLAGS) -S -I$M -I. table.c
	:rofix table.s
	as -o table.o table.s
	rm -f table.s
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
	rm *.o comp
clean:
	rm *.o comp
lintall:
	lint -hpv -I. -I$M  $M/cgram.c $M/xdefs.c $M/scan.c $M/pftn.c $M/trees.c $M/optim.c code.c local.c $M/reader.c local2.c order.c $M/match.c $M/allo.c $M/comm1.c table.c
fort: comp fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o
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
	cc -S $(CFLAGS) -I$M -I. $M/ftable.c
	:rofix ftable.s
	as -o ftable.o ftable.s
	rm -f ftable.s
forder.o: order.o
	cc -c $(CFLAGS) -I$M -I. $M/forder.c
flocal2.o: local2.o
	cc -c $(CFLAGS) -I$M -I. $M/flocal2.c
fcomm2.o: $M/common
	cc -c $(CFLAGS) -I$M -I. $M/fcomm2.c
fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o: $M/mfile2 $M/manifest macdefs mac2defs
install: comp fort
	install -s comp $(DESTDIR)/lib/$(INSTALL)
	install -s fort $(DESTDIR)/lib/$(FINSTALL)
