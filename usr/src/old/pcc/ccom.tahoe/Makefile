#
#	Makefile	1.3	86/07/27
#
# Makefile for tahoe pcc
#
# These symbols are used to configure the compiler:
#	ASSTRINGS	assembler handles string initializations
#	STABDOT		assembler understands .stabd
#	LCOMM		assembler supports .lcomm
#	FIXSTRUCT	no trickery {just output structure}
#
# Some symbols are specific to certain compilers:
#	ONEPASS		compile pass 1 and pass 2 together
#	SPRECC		do single precision in single not double
#	FORT		get f77-style pass 2
#
CONFIG=	-DASSTRINGS -DSTABDOT -DLCOMM "-DFIXSTRUCT=outstruct"
O=	-O
CFLAGS = ${O} -I. -I${M} ${CONFIG}

COPTS=	-DONEPASS ${CFLAGS}
SCOPTS=	-DONEPASS -DSPRECC ${CFLAGS}
FOPTS=	-DFORT ${CFLAGS}
LDFLAGS =

RM=	/bin/rm -f

M=	../mip
SRCS=	code.c local.c local2.c order.c rel.c stab.c table.c \
	$M/allo.c $M/common.c $M/fort.c $M/match.c $M/optim.c \
	$M/pftn.c $M/reader.c $M/scan.c $M/trees.c $M/xdefs.c \
	$M/cgram.y
# objects common to single and double precision compilers
OBJS=	rodata.o cgram.o xdefs.o scan.o pftn.o optim.o code.o reader.o \
	match.o comm1.o stab.o local.o local2.o order.o
# double precision objects
COBJS=	trees.o allo.o table.o
# single precision stuff
SFILES=	strees.c sallo.c stable.c
SOBJS=	strees.o sallo.o stable.o
# fortran version
FFILES=	freader.c fallo.c fmatch.c ftable.c forder.c flocal2.c fcomm2.c ftable.c
FOBJS=	freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o ftable.o

P1INCS=	$M/pass1.h macdefs.h $M/manifest.h pcclocal.h $M/config.h $M/ndu.h \
	/usr/include/stdio.h
P1ONEP=	${P1INCS} $M/onepass.h
P2INCS=	$M/pass2.h macdefs.h mac2defs.h $M/manifest.h pcclocal.h $M/config.h \
	$M/ndu.h /usr/include/stdio.h
P2ONEP=	${P2INCS} $M/onepass.h

TESTDIR=.

all:	ccom sccom f1

#
# 'ccom' is a one-pass C compiler.
#
ccom:	${OBJS} ${COBJS} rel.o
	${CC} ${LDFLAGS} -o ${TESTDIR}/ccom ${OBJS} ${COBJS} rel.o
trees.o: ${P1ONEP} $M/trees.c
	${CC} -c ${COPTS} $M/trees.c
optim.o: ${P1ONEP} $M/optim.c
	${CC} -c ${COPTS} $M/optim.c
pftn.o: ${P1ONEP} $M/pftn.c
	${CC} -c ${COPTS} $M/pftn.c
code.o: ${P1ONEP} code.c
	${CC} -c ${COPTS} code.c
local.o: ${P1ONEP} local.c
	${CC} -c ${COPTS} local.c
scan.o: ${P1ONEP} $M/scan.c
	${CC} -c ${COPTS} $M/scan.c
xdefs.o: ${P1ONEP} $M/xdefs.c
	${CC} -c ${COPTS} $M/xdefs.c
cgram.o: ${P1ONEP} cgram.c
	${CC} -c ${COPTS} cgram.c
rodata.o: rodata.c
	${CC} -c ${COPTS} -R rodata.c
rodata.c cgram.c: $M/cgram.y pcctokens
	cat pcctokens $M/cgram.y > gram.in
	${YACC} gram.in
	${RM} rodata.c
	sh ./:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c
comm1.o: ${P1ONEP} $M/common.c
	ln $M/common.c comm1.c
	${CC} -c ${COPTS} -DPASS1COMMON comm1.c
	${RM} comm1.c
stab.o: ${P1ONEP} stab.c \
	/usr/include/stab.h /usr/include/a.out.h /usr/include/sys/types.h
	${CC} -c ${COPTS} stab.c
table.o: ${P2ONEP} table.c
	${CC} -c ${COPTS} -R table.c
reader.o: ${P2ONEP} $M/reader.c
	${CC} -c ${COPTS} $M/reader.c
local2.o: ${P2ONEP} local2.c
	${CC} -c ${COPTS} local2.c
order.o: ${P2ONEP} order.c
	${CC} -c ${COPTS} order.c
match.o: ${P2ONEP} $M/match.c
	${CC} -c ${COPTS} $M/match.c
allo.o: ${P2ONEP} $M/allo.c
	${CC} -c ${COPTS} $M/allo.c

#
# 'sccom' is a C compiler that does single precision computations in
#	single precision rather than double precision.
#
sccom:	${OBJS} ${SOBJS} rel.o
	${CC} ${LDFLAGS} -o ${TESTDIR}/sccom ${OBJS} ${SOBJS} rel.o
strees.o: ${P1ONEP} $M/trees.c
	ln $M/trees.c strees.c
	${CC} -c ${SCOPTS} strees.c
	${RM} strees.c
sallo.o: ${P2ONEP} $M/allo.c
	ln $M/allo.c sallo.c
	${CC} -c ${SCOPTS} sallo.c
	${RM} sallo.c
stable.o: ${P2ONEP} table.c
	ln table.c stable.c
	${CC} -c ${SCOPTS} -R stable.c
	${RM} stable.c

#
# 'f1' is the f77 and pc code generator.
#
f1:	fort.o ${FOBJS}
	${CC} ${LDFLAGS} -o ${TESTDIR}/f1 fort.o ${FOBJS}
fort.o: ${P2INCS} fort.h $M/fort.c
	${CC} -c ${FOPTS} $M/fort.c
freader.o: ${P2INCS} $M/reader.c
	ln $M/reader.c freader.c
	${CC} -c ${FOPTS} freader.c
	${RM} freader.c
fallo.o: ${P2INCS} $M/allo.c
	ln $M/allo.c fallo.c
	${CC} -c ${FOPTS} fallo.c
	${RM} fallo.c
fmatch.o: ${P2INCS} $M/match.c
	ln $M/match.c fmatch.c
	${CC} -c ${FOPTS} fmatch.c
	${RM} fmatch.c
ftable.o: ${P2INCS} table.c
	ln table.c ftable.c
	${CC} -c -R ${FOPTS} ftable.c
	${RM} ftable.c
forder.o: ${P2INCS} order.c
	ln order.c forder.c
	${CC} -c ${FOPTS} forder.c
	${RM} forder.c
flocal2.o: ${P2INCS} local2.c
	ln local2.c flocal2.c
	${CC} -c ${FOPTS} flocal2.c
	${RM} flocal2.c
fcomm2.o: ${P2INCS} $M/common.c
	ln $M/common.c fcomm2.c
	${CC} -c ${FOPTS} -DPASS2COMMON fcomm2.c
	${RM} fcomm2.c

install: all
	install -s ${TESTDIR}/ccom ${DESTDIR}/lib/ccom
	install -s ${TESTDIR}/sccom ${DESTDIR}/lib/sccom
	install -s ${TESTDIR}/f1 ${DESTDIR}/lib/f1

GREP=	egrep

pcclocal.h: localdefs.h /usr/include/pcc.h
	${RM} pcclocal.h
	cat /usr/include/pcc.h localdefs.h | ${GREP} '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | sed -e 's/PCC[A-Z]*_//' > pcclocal.h 

pcctokens: localdefs.h /usr/include/pcc.h
	${RM} pcctokens
	cat /usr/include/pcc.h localdefs.h | ${GREP} '^#[ 	]*define[ 	][ 	]*PCC_' | sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > pcctokens

DUMMIES=	${SFILES} ${FFILES}

shrink:
	${RM} *.o ccom sccom f1 ${DUMMIES}
clean:
	${RM} *.o ccom sccom f1 cgram.c rodata.c pcctokens pcclocal.h gram.in ${DUMMIES}
sfix:
	${RM} ${SFILES}
sclean:
	${RM} ${SOBJS} ${SFILES}
ffix:
	${RM} ${FFILES}
fclean:
	${RM} ${FOBJS} ${FFILES}

lintall:
	lint -hax -I. -I$M  cgram.c $M/xdefs.c $M/scan.c $M/pftn.c \
		$M/trees.c $M/optim.c code.c local.c stab.c $M/reader.c \
		local2.c order.c $M/match.c $M/allo.c $M/comm1.c table.c

tags:	${SRCS}
	ctags ${SRCS}
