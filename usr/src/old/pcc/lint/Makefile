
#
#	@(#)Makefile	1.2	(Berkeley)	%G%
#
M=/usr/src/lib/mip
LOAD1=-s
LOAD2=-s
CFLAGS=-O -DFLEXNAMES
head: lpass1 lpass2 llib-port.ln llib-lc.ln;
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
llib-port.ln: llib-port lpass1
	-(/lib/cpp -C -Dlint llib-port | ./lpass1 -puv > llib-port.ln )
llib-lc.ln: llib-lc lpass1
	-(/lib/cpp -C -Dlint llib-lc | ./lpass1 -v > llib-lc.ln )
shrink:
	rm *.o
clean: shrink
	rm lpass1 lpass2 cgram.c llib-lc.ln llib-port.ln
lpass2: lpass2.o
	cc $(LOAD2) lpass2.o
	mv a.out lpass2
lpass2.o: $M/manifest lmanifest
	cc $(CFLAGS) -c -I$M -I. lpass2.c
lintall:
	lint -hpv -I. -I$M  $M/cgram.c $M/xdefs.c $M/scan.c $M/pftn.c $M/trees.c $M/optim.c lint.c
install: head SHELL
	cp lpass1 ${DESTDIR}/usr/lib/lint/lint1
	cp lpass2 ${DESTDIR}/usr/lib/lint/lint2
	cp llib-* ${DESTDIR}/usr/lib/lint
	cp SHELL ${DESTDIR}/usr/bin/lint

sources: READ_ME SHELL lint.c llib-lc llib-port lmanifest lpass2.c macdefs

READ_ME SHELL lint.c llib-lc llib-port lmanifest lpass2.c macdefs:
	sccs get $@
