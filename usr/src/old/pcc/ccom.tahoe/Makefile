#	Makefile	1.1	86/01/11
#
# Makefile for portable c compiler
#
DESTDIR=
M=	../mip.tahoe
CFLAGS=	-O -I${M} -I.
OBJS=	rodata.o cgram.o xdefs.o scan.o pftn.o trees.o optim.o code.o local.o \
	reader.o local2.o order.o match.o allo.o comm1.o table.o stab.o

all:	ccom alist

ccom: 	${OBJS}
	$(CC) $(LDFLAGS) ${OBJS} -o ccom
alist: alist.c
	$(CC) -o alist $(CFLAGS)  alist.c
	
trees.o: $M/manifest macdefs $M/mfile1 $M/trees.c
	$(CC) -c $(CFLAGS)  $M/trees.c
optim.o: $M/manifest macdefs $M/mfile1 $M/optim.c
	$(CC) -c $(CFLAGS) $M/optim.c
pftn.o: $M/manifest macdefs $M/mfile1 $M/pftn.c
	$(CC) -c $(CFLAGS) $M/pftn.c
code.o: $M/manifest macdefs $M/mfile1
	$(CC) -c $(CFLAGS) code.c
local.o: $M/manifest macdefs $M/mfile1
	$(CC) -c $(CFLAGS) local.c
stab.o: $M/manifest macdefs $M/mfile1
	$(CC) -c $(CFLAGS) stab.c
scan.o: $M/manifest macdefs $M/mfile1 $M/scan.c
	$(CC) -c $(CFLAGS) $M/scan.c
xdefs.o: $M/manifest $M/mfile1 macdefs $M/xdefs.c
	$(CC) -c $(CFLAGS) $M/xdefs.c
cgram.o: $M/manifest $M/mfile1 macdefs cgram.c
	$(CC) -c $(CFLAGS) cgram.c
rodata.o: rodata.c
	$(CC) -c $(CFLAGS) -R rodata.c
rodata.c cgram.c: $M/cgram.y
	$(YACC) $M/cgram.y
	rm -f rodata.c
	./:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c
comm1.o: $M/manifest $M/mfile1 $M/common macdefs $M/comm1.c
	$(CC) -c $(CFLAGS) $M/comm1.c
table.o: $M/manifest $M/mfile2 mac2defs macdefs table.c
	$(CC) -c $(CFLAGS) -R table.c
reader.o: $M/manifest $M/mfile2 mac2defs macdefs $M/reader.c
	$(CC) -c $(CFLAGS) $M/reader.c
local2.o: $M/manifest $M/mfile2 mac2defs macdefs
	$(CC) -c $(CFLAGS) local2.c
order.o: $M/manifest $M/mfile2 mac2defs macdefs
	$(CC) -c $(CFLAGS) order.c
match.o: $M/manifest $M/mfile2 mac2defs macdefs $M/match.c
	$(CC) -c $(CFLAGS) $M/match.c
allo.o: $M/manifest $M/mfile2 mac2defs macdefs $M/allo.c
	$(CC) -c $(CFLAGS) $M/allo.c
clean:
	rm -f *.o ccom cgram.c rodata.c
lintall:
	lint -hv -I. -I$M $(CFLAGS) rodata.c cgram.c $M/xdefs.c \
	$M/scan.c $M/pftn.c $M/trees.c $M/optim.c code.c local.c \
	$M/reader.c local2.c order.c $M/match.c $M/allo.c $M/comm1.c table.c

install: all
	-mv ${DESTDIR}/lib/ccom ${DESTDIR}/lib/ccom.old
	install -s ccom ${DESTDIR}/lib/ccom
	install -s alist ${DESTDIR}/lib/alist
