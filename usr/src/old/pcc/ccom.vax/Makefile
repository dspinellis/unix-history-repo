#
#	Makefile	1.19	89/03/06
#
# Makefile for PCC
#
# Some symbols are specific to certain compilers:
#	ONEPASS		compile pass 1 and pass 2 together
#	SPRECC		do single precision in single not double
#	FORT		get f77-style pass 2
#
O = -O
CFLAGS = $(O) -I. -I$(M)

COPTS = -DONEPASS $(CFLAGS)
SCOPTS = -DONEPASS -DSPRECC $(CFLAGS)
FOPTS = -DFORT $(CFLAGS)
LDFLAGS =

RM=	/bin/rm -f
LN=	/bin/ln -s

SFILES=	strees.c slocal.c slocal2.c sallo.c sorder.c stable.c
SOBJS=	strees.o slocal.o slocal2.o sallo.o sorder.o stable.o
FFILES=	freader.c fallo.c fmatch.c ftable.c forder.c flocal2.c fcomm2.c
FOBJS=	freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o

P1INCS=	$M/pass1.h macdefs.h $M/manifest.h pcclocal.h $M/config.h $M/ndu.h \
	/usr/include/stdio.h
P1ONEP=	$(P1INCS) $M/onepass.h
P2INCS=	$M/pass2.h macdefs.h mac2defs.h $M/manifest.h pcclocal.h $M/config.h \
	$M/ndu.h /usr/include/stdio.h
P2ONEP=	$(P2INCS) $M/onepass.h
OBJS=	code.o rel.o stab.o comm1.o match.o optim.o pftn.o \
	reader.o scan.o xdefs.o cgram.o rodata.o
DOBJS=	allo.o local.o local2.o order.o trees.o table.o
M=../../mip
TESTDIR = .

all: ccom sccom f1

#
# 'ccom' is a one-pass C compiler.
#
ccom:	${OBJS} ${DOBJS}
	$(CC) $(LDFLAGS) ${OBJS} ${DOBJS} -o $(TESTDIR)/ccom
trees.o: $(P1ONEP) $M/trees.c
	$(CC) -c $(COPTS) $M/trees.c
optim.o: $(P1ONEP) $M/optim.c
	$(CC) -c $(COPTS) $M/optim.c
pftn.o: $(P1ONEP) $M/pftn.c
	$(CC) -c $(COPTS) $M/pftn.c
code.o: $(P1ONEP) code.c
	$(CC) -c $(COPTS) code.c
local.o: $(P1ONEP) local.c
	$(CC) -c $(COPTS) local.c
scan.o: $(P1ONEP) $M/scan.c
	$(CC) -c $(COPTS) $M/scan.c
xdefs.o: $(P1ONEP) $M/xdefs.c
	$(CC) -c $(COPTS) $M/xdefs.c
cgram.o: $(P1ONEP) cgram.c
	$(CC) -c $(COPTS) cgram.c
rodata.o: rodata.c
	$(CC) -c $(COPTS) -R rodata.c
rodata.c cgram.c: $M/cgram.y pcctokens
	cat pcctokens $M/cgram.y > gram.in
	$(YACC) gram.in
	$(RM) rodata.c
	sh ./:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c
comm1.o: $(P1ONEP) $M/common.c
	$(LN) $M/common.c comm1.c
	$(CC) -c $(COPTS) -DPASS1COMMON comm1.c
	$(RM) comm1.c
stab.o: $(P1ONEP) stab.c \
	/usr/include/stab.h /usr/include/a.out.h /usr/include/sys/types.h
	$(CC) -c $(COPTS) stab.c
table.o: $(P2ONEP) table.c
	$(CC) -c $(COPTS) -R table.c
reader.o: $(P2ONEP) $M/reader.c
	$(CC) -c $(COPTS) $M/reader.c
local2.o: $(P2ONEP) local2.c
	$(CC) -c $(COPTS) local2.c
order.o: $(P2ONEP) order.c
	$(CC) -c $(COPTS) order.c
match.o: $(P2ONEP) $M/match.c
	$(CC) -c $(COPTS) $M/match.c
allo.o: $(P2ONEP) $M/allo.c
	$(CC) -c $(COPTS) $M/allo.c

#
# 'sccom' is a C compiler that does single precision computations in
#	single precision rather than double precision.
#
sccom:	${OBJS} ${SOBJS}
	$(CC) $(LDFLAGS) ${OBJS} ${SOBJS} -o $(TESTDIR)/sccom
strees.o: $(P1ONEP) $M/trees.c
	$(LN) $M/trees.c strees.c
	$(CC) -c $(SCOPTS) strees.c
	$(RM) strees.c
slocal.o: $(P1ONEP) local.c
	$(LN) local.c slocal.c
	$(CC) -c $(SCOPTS) slocal.c
	$(RM) slocal.c
slocal2.o: $(P2ONEP) local2.c
	$(LN) local2.c slocal2.c
	$(CC) -c $(SCOPTS) slocal2.c
	$(RM) slocal2.c
sallo.o: $(P2ONEP) $M/allo.c
	$(LN) $M/allo.c sallo.c
	$(CC) -c $(SCOPTS) sallo.c
	$(RM) sallo.c
sorder.o: $(P2ONEP) order.c
	$(LN) order.c sorder.c
	$(CC) -c $(SCOPTS) sorder.c
	$(RM) sorder.c
stable.o: $(P2ONEP) table.c
	$(LN) table.c stable.c
	$(CC) -c $(SCOPTS) -R stable.c
	$(RM) stable.c

#
# 'f1' is the f77 and pc code generator.
#
f1: fort.o freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o \
	fcomm2.o
	$(CC) $(LDFLAGS) fort.o freader.o fallo.o fmatch.o ftable.o \
		forder.o flocal2.o fcomm2.o -o $(TESTDIR)/f1
fort.o: $(P2INCS) fort.h $M/fort.c
	$(CC) -c $(FOPTS) $M/fort.c
freader.o: $(P2INCS) $M/reader.c
	$(LN) $M/reader.c freader.c
	$(CC) -c $(FOPTS) freader.c
	$(RM) freader.c
fallo.o: $(P2INCS) $M/allo.c
	$(LN) $M/allo.c fallo.c
	$(CC) -c $(FOPTS) fallo.c
	$(RM) fallo.c
fmatch.o: $(P2INCS) $M/match.c
	$(LN) $M/match.c fmatch.c
	$(CC) -c $(FOPTS) fmatch.c
	$(RM) fmatch.c
ftable.o: $(P2INCS) table.c
	$(LN) table.c ftable.c
	$(CC) -c -R $(FOPTS) ftable.c
	$(RM) ftable.c
forder.o: $(P2INCS) order.c
	$(LN) order.c forder.c
	$(CC) -c $(FOPTS) forder.c
	$(RM) forder.c
flocal2.o: $(P2INCS) local2.c
	$(LN) local2.c flocal2.c
	$(CC) -c $(FOPTS) flocal2.c
	$(RM) flocal2.c
fcomm2.o: $(P2INCS) $M/common.c
	$(LN) $M/common.c fcomm2.c
	$(CC) -c $(FOPTS) -DPASS2COMMON fcomm2.c
	$(RM) fcomm2.c

install: all
	install -s -o bin -g bin -m 755 ${TESTDIR}/ccom ${DESTDIR}/lib/ccom
	install -s -o bin -g bin -m 755 ${TESTDIR}/sccom ${DESTDIR}/lib/sccom
	install -s -o bin -g bin -m 755 ${TESTDIR}/f1 ${DESTDIR}/lib/f1

GREP=	egrep

pcclocal.h: ../localdefs.h /usr/include/pcc.h
	$(RM) pcclocal.h
	cat /usr/include/pcc.h ../localdefs.h | $(GREP) '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | sed -e 's/PCC[A-Z]*_//' > pcclocal.h 

pcctokens: ../localdefs.h /usr/include/pcc.h
	$(RM) pcctokens
	cat /usr/include/pcc.h ../localdefs.h | $(GREP) '^#[ 	]*define[ 	][ 	]*PCC_' | sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > pcctokens

DUMMIES=	$(SFILES) $(FFILES)

shrink:
	$(RM) *.o ccom sccom f1 $(DUMMIES)
clean:
	$(RM) *.o ccom sccom f1 cgram.c rodata.c pcctokens pcclocal.h gram.in $(DUMMIES)
cleandir: clean
	rm -f ${MAN} tags .depend
sfix:
	$(RM) $(SFILES)
sclean:
	$(RM) $(SOBJS) $(SFILES)
ffix:
	$(RM) $(FFILES)
fclean:
	$(RM) $(FOBJS) $(FFILES)

# Should use -a, but the situation is too confusing to fix at the moment...
lintall:
	lint -hx $(COPTS) -DPASS1COMMON cgram.c $M/xdefs.c $M/scan.c \
		$M/pftn.c $M/trees.c $M/optim.c code.c local.c stab.c \
		$M/reader.c local2.c order.c $M/match.c $M/allo.c \
		$M/common.c table.c rodata.c rel.c

depend:
lint:
tags:
