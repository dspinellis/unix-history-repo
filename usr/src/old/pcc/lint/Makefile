#
#	@(#)Makefile	1.23	(Berkeley)	%G%
#
M=../../lib/mip
P=../../lib/pcc
CFLAGS=-O
LINTLIBS=llib-port.ln llib-lc.ln llib-lcurses.ln llib-ldbm.ln llib-lm.ln \
	llib-lmp.ln llib-lplot.ln llib-ltermcap.ln
SRCS=	$M/cgram.y $M/common.c $M/optim.c $M/pftn.c $M/scan.c $M/trees.c \
	$M/xdefs.c lint.c hash.c $M/pass1.h $M/manifest.h macdefs.h \
	lpass2.c
MAN=	lint.0

all: lpass1 lpass2 ${LINTLIBS}

lpass1: cgram.o rodata.o comm1.o optim.o pftn.o scan.o trees.o xdefs.o \
	lint.o hash.o rel.o
	${CC} ${CFLAGS} cgram.o rodata.o comm1.o optim.o pftn.o scan.o \
		trees.o xdefs.o lint.o hash.o rel.o -o lpass1

cgram.o: $M/manifest.h $M/pass1.h pcclocal.h macdefs.h cgram.c
	$(CC) -c $(CFLAGS) -I$M -I. cgram.c

rodata.o: rodata.c
	$(CC) -c $(CFLAGS) -R rodata.c

comm1.o: $M/manifest.h $M/pass1.h pcclocal.h $M/common.c macdefs.h
	ln -s $M/common.c comm1.c
	$(CC) -c $(CFLAGS) -I$M -I. -DPASS1COMMON comm1.c
	rm -f comm1.c

optim.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/optim.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/optim.c

pftn.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/pftn.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/pftn.c

scan.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/scan.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/scan.c

trees.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/trees.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/trees.c

xdefs.o: $M/manifest.h $M/pass1.h pcclocal.h macdefs.h $M/xdefs.c
	$(CC) -c $(CFLAGS) -I$M -I. $M/xdefs.c

rodata.c cgram.c: $M/cgram.y pcctokens
	cat pcctokens $M/cgram.y > gram.in
	$(YACC) gram.in
	rm -f rodata.c
	sh ./:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c

GREP=	egrep

pcclocal.h: $P/localdefs.h /usr/include/pcc.h
	rm -f pcclocal.h
	cat /usr/include/pcc.h $P/localdefs.h | $(GREP) '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | sed -e 's/PCC[A-Z]*_//' > pcclocal.h 

pcctokens: $P/localdefs.h /usr/include/pcc.h
	rm -f pcctokens
	cat /usr/include/pcc.h $P/localdefs.h | $(GREP) '^#[ 	]*define[ 	][ 	]*PCC_' | sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > pcctokens

lint.o: $M/manifest.h macdefs.h $M/pass1.h lmanifest.h
	${CC} -c ${CFLAGS} -I$M -I. lint.c

llib-port.ln: llib-port lpass1
	-(/lib/cpp -C -Dlint llib-port | ./lpass1 -puv > llib-port.ln )

llib-ldbm.ln: llib-ldbm lpass1
	-(/lib/cpp -C -Dlint llib-ldbm | ./lpass1 -uv > llib-ldbm.ln )

llib-lm.ln: llib-lm lpass1
	-(/lib/cpp -C -Dlint llib-lm | ./lpass1 -uv > llib-lm.ln )

llib-lmp.ln: llib-lmp lpass1
	-(/lib/cpp -C -Dlint llib-lmp | ./lpass1 -uv > llib-lmp.ln )

llib-lplot.ln: llib-lplot lpass1
	-(/lib/cpp -C -Dlint llib-lplot | ./lpass1 -uv > llib-lplot.ln )

llib-ltermcap.ln: llib-ltermcap lpass1
	-(/lib/cpp -C -Dlint llib-ltermcap | ./lpass1 -uv > llib-ltermcap.ln )

llib-lc.ln: llib-lc lpass1
	-(/lib/cpp -C -Dlint llib-lc | ./lpass1 -v > llib-lc.ln )

llib-lcurses.ln: llib-lcurses lpass1
	-(/lib/cpp -C -Dlint -D__void__=__curses__ llib-lcurses | ./lpass1 -v > llib-lcurses.ln )

lpass2: lpass2.o hash.o
	${CC} ${CFLAGS} lpass2.o hash.o -o lpass2

lpass2.o: $M/manifest.h lmanifest.h pcclocal.h macdefs.h
	${CC} ${CFLAGS} -c -I$M -I. lpass2.c

hash.o: hash.c
	${CC} ${CFLAGS} -c -I$M -I. hash.c

lintall:
	lint -hpv -I. -I$M  $M/cgram.c $M/xdefs.c $M/scan.c \
		$M/pftn.c $M/trees.c $M/optim.c lint.c

install: all SHELL ${MAN}
	install -s -o bin -g bin -m 755 lpass1 ${DESTDIR}/usr/lib/lint/lint1
	install -s -o bin -g bin -m 755 lpass2 ${DESTDIR}/usr/lib/lint/lint2
	install -c -o bin -g bin -m 444 llib-* ${DESTDIR}/usr/lib/lint
	-rm -f ${DESTDIR}/usr/lib/lint/llib-ltermlib*
	ln -s llib-ltermcap ${DESTDIR}/usr/lib/lint/llib-ltermlib
	ln -s llib-ltermcap.ln ${DESTDIR}/usr/lib/lint/llib-ltermlib.ln
	install -c -o bin -g bin -m 755 SHELL ${DESTDIR}/usr/bin/lint
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

shrink:
	rm -f *.o errs rodata.c pcctokens pcclocal.h

clean: shrink
	rm -f lpass1 lpass2 cgram.c gram.in tags ${LINTLIBS}

cleandir: clean
	rm -f ${MAN} tags .depend

tags: ${SRCS}
	ctags -w ${SRCS}

depend lint:
