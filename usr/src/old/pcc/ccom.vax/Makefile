#
#	Makefile	1.10	85/04/08
#
# Makefile for PCC
#
# These symbols are used to configure the compiler:
#	ASSTRINGS	assembler handles string initializations
#	STABDOT		assembler understands .stabd
#	LCOMM		assembler supports .lcomm
#	FIXSTRUCT	no trickery (just output structure)
#
# Some symbols are specific to certain compilers:
#	ONEPASS		compile pass 1 and pass 2 together
#	SPRECC		do single precision in single not double
#	FORT		get f77-style pass 2
#
CONFIG=	-DASSTRINGS -DSTABDOT -DLCOMM "-DFIXSTRUCT=outstruct"
O = -O
CFLAGS = $(O) -I. -I$(M) $(CONFIG)

COPTS = -DONEPASS $(CFLAGS)
SCOPTS = -DONEPASS -DSPRECC $(CFLAGS)
FOPTS = -DFORT $(CFLAGS)
LDFLAGS = -g

RM=	/bin/rm -f

SFILES=	strees.c slocal.c slocal2.c sallo.c sorder.c stable.c
SOBJS=	strees.o slocal.o slocal2.o sallo.o sorder.o stable.o
FFILES=	freader.c fallo.c fmatch.c ftable.c forder.c flocal2.c fcomm2.c
FOBJS=	freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o

M=../mip
TESTDIR = .

all: comp scomp fort

#
# 'comp' is a one-pass C compiler.
#
comp: rodata.o cgram.o xdefs.o scan.o pftn.o trees.o optim.o code.o local.o \
	reader.o local2.o order.o match.o allo.o comm1.o table.o stab.o
	$(CC) $(LDFLAGS) rodata.o cgram.o xdefs.o scan.o pftn.o trees.o \
		optim.o code.o local.o reader.o local2.o order.o match.o \
		allo.o comm1.o table.o stab.o -o $(TESTDIR)/comp
trees.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/trees.c
	$(CC) -c $(COPTS) -I$M -I.  $M/trees.c
optim.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/optim.c
	$(CC) -c $(COPTS) -I$M -I. $M/optim.c
pftn.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/pftn.c
	$(CC) -c $(COPTS) -I$M -I. $M/pftn.c
code.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h
	$(CC) -c $(COPTS) -I$M -I. code.c
local.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h
	$(CC) -c $(COPTS) -I$M -I. local.c
scan.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/scan.c
	$(CC) -c $(COPTS) -I$M -I. $M/scan.c
xdefs.o: $M/manifest.h $M/pass1.h pcclocal.h macdefs.h $M/xdefs.c
	$(CC) -c $(COPTS) -I$M -I. $M/xdefs.c
cgram.o: $M/manifest.h $M/pass1.h pcclocal.h macdefs.h cgram.c
	$(CC) -c $(COPTS) -I$M -I. cgram.c
rodata.o: rodata.c
	$(CC) -c $(COPTS) -R rodata.c
rodata.c cgram.c: $M/cgram.y pcctokens
	cat pcctokens $M/cgram.y > gram.in
	$(YACC) gram.in
	$(RM) rodata.c
	sh ./:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c
comm1.o: $M/manifest.h $M/pass1.h pcclocal.h $M/common.c macdefs.h
	ln $M/common.c comm1.c
	$(CC) -c $(COPTS) -I$M -I. -DPASS1COMMON comm1.c
	$(RM) comm1.c
table.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h table.c
	$(CC) -c $(COPTS) -R -I$M -I. table.c
reader.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/reader.c
	$(CC) -c $(COPTS) -I$M -I. $M/reader.c
local2.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h
	$(CC) -c $(COPTS) -I$M -I. local2.c
order.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h
	$(CC) -c $(COPTS) -I$M -I. order.c
match.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/match.c
	$(CC) -c $(COPTS) -I$M -I. $M/match.c
allo.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/allo.c
	$(CC) -c $(COPTS) -I$M -I. $M/allo.c

#
# 'scomp' is a C compiler that does single precision computations in
#	single precision rather than double precision.
#
scomp: rodata.o cgram.o xdefs.o scan.o pftn.o strees.o optim.o code.o slocal.o \
	reader.o slocal2.o sorder.o match.o sallo.o comm1.o stable.o stab.o
	$(CC) $(LDFLAGS) rodata.o cgram.o xdefs.o scan.o pftn.o strees.o \
		optim.o code.o slocal.o reader.o slocal2.o sorder.o match.o \
		sallo.o comm1.o stable.o stab.o -o $(TESTDIR)/scomp
strees.o: $M/manifest.h macdefs.h $M/pass1.h pcclocal.h $M/trees.c
	ln $M/trees.c strees.c
	$(CC) -c $(SCOPTS) -I$M -I. strees.c
	$(RM) strees.c
slocal.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h local.c
	ln local.c slocal.c
	$(CC) -c $(SCOPTS) -I$M -I. slocal.c
	$(RM) slocal.c
slocal2.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h local2.c
	ln local2.c slocal2.c
	$(CC) -c $(SCOPTS) -I$M -I. slocal2.c
	$(RM) slocal2.c
sallo.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/allo.c
	ln $M/allo.c sallo.c
	$(CC) -c $(SCOPTS) -I$M -I. sallo.c
	$(RM) sallo.c
sorder.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h order.c
	ln order.c sorder.c
	$(CC) -c $(SCOPTS) -I$M -I. sorder.c
	$(RM) sorder.c
stable.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h table.c
	ln table.c stable.c
	$(CC) -c $(SCOPTS) -R -I$M -I. stable.c
	$(RM) stable.c

#
# 'fort' is the f77 and pc code generator.
#
fort: fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o \
	fcomm2.o
	$(CC) $(LDFLAGS) fort.o freader.o fallo.o fmatch.o ftable.o \
		forder.o flocal2.o fcomm2.o -o $(TESTDIR)/fort
fort.o: fort.h $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/fort.c
	$(CC) -c $(FOPTS) -I$M -I. $M/fort.c
freader.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/reader.c
	ln $M/reader.c freader.c
	$(CC) -c $(FOPTS) -I$M -I. freader.c
	$(RM) freader.c
fallo.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/allo.c
	ln $M/allo.c fallo.c
	$(CC) -c $(FOPTS) -I$M -I. fallo.c
	$(RM) fallo.c
fmatch.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h $M/match.c
	ln $M/match.c fmatch.c
	$(CC) -c $(FOPTS) -I$M -I. fmatch.c
	$(RM) fmatch.c
ftable.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h table.c
	ln table.c ftable.c
	$(CC) -c -R $(FOPTS) -I$M -I. ftable.c
	$(RM) ftable.c
forder.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h
	ln order.c forder.c
	$(CC) -c $(FOPTS) -I$M -I. forder.c
	$(RM) forder.c
flocal2.o: $M/manifest.h $M/pass2.h pcclocal.h mac2defs.h macdefs.h
	ln local2.c flocal2.c
	$(CC) -c $(FOPTS) -I$M -I. flocal2.c
	$(RM) flocal2.c
fcomm2.o: $M/manifest.h $M/pass2.h pcclocal.h $M/common.c mac2defs.h macdefs.h
	ln $M/common.c fcomm2.c
	$(CC) -c $(FOPTS) -I$M -I. -DPASS2COMMON fcomm2.c
	$(RM) fcomm2.c

install:	all
	install comp ${DESTDIR}/lib/ccom
	install scomp ${DESTDIR}/lib/sccom
	install fort ${DESTDIR}/lib/f1

GREP=	egrep

pcclocal.h: localdefs.h /usr/include/pcc.h
	$(RM) pcclocal.h
	cat /usr/include/pcc.h localdefs.h | $(GREP) '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | sed -e 's/PCC[A-Z]*_//' > pcclocal.h 

pcctokens: localdefs.h /usr/include/pcc.h
	$(RM) pcctokens
	cat /usr/include/pcc.h localdefs.h | $(GREP) '^#[ 	]*define[ 	][ 	]*PCC_' | sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > pcctokens

DUMMIES=	$(SFILES) $(FFILES)

shrink:
	$(RM) *.o comp scomp fort $(DUMMIES)
clean:
	$(RM) *.o comp scomp fort cgram.c rodata.c pcctokens pcclocal.h gram.in $(DUMMIES)
sfix:
	$(RM) $(SFILES)
sclean:
	$(RM) $(SOBJS) $(SFILES)
ffix:
	$(RM) $(FFILES)
fclean:
	$(RM) $(FOBJS) $(FFILES)

lintall:
	lint -hv -I. -I$M  cgram.c $M/xdefs.c $M/scan.c $M/pftn.c \
		$M/trees.c $M/optim.c code.c local.c $M/reader.c \
		local2.c order.c $M/match.c $M/allo.c $M/comm1.c table.c
