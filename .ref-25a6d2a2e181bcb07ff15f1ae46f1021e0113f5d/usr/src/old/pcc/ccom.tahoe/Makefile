#
#	Makefile	1.12	89/05/11
#
# Makefile for tahoe pcc
#
RM=	/bin/rm -f
GREP=	egrep
MIP=	../../mip
#
# Some symbols are specific to certain compilers:
#	ONEPASS		compile pass 1 and pass 2 together
#	FORT		get f77-style pass 2
#
INCS=	-I. -I${MIP}
CFLAGS=	-O
LDFLAGS=
LIBS=

# c version
OPTS=	-DONEPASS ${INCS}
FLAGS=	${CFLAGS} ${OPTS}
SRCS=	code.c local.c local2.c order.c rel.c stab.c \
	${MIP}/common.c ${MIP}/match.c ${MIP}/optim.c ${MIP}/pftn.c \
	${MIP}/reader.c ${MIP}/scan.c ${MIP}/xdefs.c ${MIP}/cgram.y \
	${MIP}/allo.c ${MIP}/trees.c table.c
OBJS=	code.o local.o local2.o order.o rel.o stab.o \
	comm1.o match.o optim.o pftn.o reader.o scan.o xdefs.o cgram.o \
	allo.o trees.o table.o rodata.o
# fortran version
FOPTS=	-DFORT ${INCS}
FFLAGS=	${CFLAGS} ${FOPTS}
FSRCS=	freader.c fallo.c fmatch.c ftable.c forder.c flocal2.c fcomm2.c ftable.c
FOBJS=	freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o ftable.o

TESTDIR=.

all:	ccom f1

#
# 'ccom' is a one-pass C compiler.
#
ccom:	${OBJS} rel.o
	${CC} ${LDFLAGS} -o ${TESTDIR}/ccom ${OBJS} rel.o ${LIBS}
trees.o: ${MIP}/trees.c
	${CC} -c ${FLAGS} ${MIP}/trees.c
optim.o: ${MIP}/optim.c
	${CC} -c ${FLAGS} ${MIP}/optim.c
pftn.o: ${MIP}/pftn.c
	${CC} -c ${FLAGS} ${MIP}/pftn.c
code.o: code.c
	${CC} -c ${FLAGS} code.c
local.o: local.c
	${CC} -c ${FLAGS} local.c
scan.o: ${MIP}/scan.c
	${CC} -c ${FLAGS} ${MIP}/scan.c
xdefs.o: ${MIP}/xdefs.c
	${CC} -c ${FLAGS} ${MIP}/xdefs.c
cgram.o: cgram.c
	${CC} -c ${FLAGS} cgram.c
rodata.o: rodata.c
	${CC} -c ${FLAGS} -R rodata.c
rodata.c cgram.c: ${MIP}/cgram.y pcctokens
	cat pcctokens ${MIP}/cgram.y > gram.in
	${YACC} gram.in
	${RM} rodata.c
	sh :yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c
comm1.o: ${MIP}/common.c
	ln ${MIP}/common.c comm1.c
	${CC} -c ${FLAGS} -DPASS1COMMON comm1.c
	${RM} comm1.c
stab.o: stab.c
	${CC} -c ${FLAGS} stab.c
table.o: table.c
	${CC} -c ${FLAGS} -R table.c
reader.o: ${MIP}/reader.c
	${CC} -c ${FLAGS} ${MIP}/reader.c
local2.o: local2.c
	${CC} -c ${FLAGS} local2.c
order.o: order.c
	${CC} -c ${FLAGS} order.c
match.o: ${MIP}/match.c
	${CC} -c ${FLAGS} ${MIP}/match.c
allo.o: ${MIP}/allo.c
	${CC} -c ${FLAGS} ${MIP}/allo.c

#
# 'f1' is the f77 and pc code generator.
#
f1:	fort.o ${FOBJS}
	${CC} ${LDFLAGS} -o ${TESTDIR}/f1 fort.o ${FOBJS} ${LIBS}
fort.o: fort.h ${MIP}/fort.c
	${CC} -c ${FFLAGS} ${MIP}/fort.c
freader.o: ${MIP}/reader.c
	ln ${MIP}/reader.c freader.c
	${CC} -c ${FFLAGS} freader.c
	${RM} freader.c
fallo.o: ${MIP}/allo.c
	ln ${MIP}/allo.c fallo.c
	${CC} -c ${FFLAGS} fallo.c
	${RM} fallo.c
fmatch.o: ${MIP}/match.c
	ln ${MIP}/match.c fmatch.c
	${CC} -c ${FFLAGS} fmatch.c
	${RM} fmatch.c
ftable.o: table.c
	ln table.c ftable.c
	${CC} -c -R ${FFLAGS} ftable.c
	${RM} ftable.c
forder.o: order.c
	ln order.c forder.c
	${CC} -c ${FFLAGS} forder.c
	${RM} forder.c
flocal2.o: local2.c
	ln local2.c flocal2.c
	${CC} -c ${FFLAGS} flocal2.c
	${RM} flocal2.c
fcomm2.o: ${MIP}/common.c
	ln ${MIP}/common.c fcomm2.c
	${CC} -c ${FFLAGS} -DPASS2COMMON fcomm2.c
	${RM} fcomm2.c

install:
	install -s -o bin -g bin -m 755 ${TESTDIR}/ccom ${DESTDIR}/usr/libexec
	install -s -o bin -g bin -m 755 ${TESTDIR}/f1 ${DESTDIR}/usr/libexec

pcclocal.h: ../localdefs.h /usr/include/pcc.h
	${RM} pcclocal.h
	cat /usr/include/pcc.h ../localdefs.h | \
	    ${GREP} '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	    sed -e 's/PCC[A-Z]*_//' > pcclocal.h 

pcctokens: ../localdefs.h /usr/include/pcc.h
	${RM} pcctokens
	cat /usr/include/pcc.h ../localdefs.h | \
	    ${GREP} '^#[ 	]*define[ 	][ 	]*PCC_' | sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > pcctokens

clean:
	${RM} *.o ccom f1 cgram.c rodata.c pcctokens pcclocal.h \
	    gram.in ${SSRCS} ${FSRCS}

cleandir: clean
	rm -f ${MAN} tags .depend

lint:
	lint -hx ${OPTS} -DPASS1COMMON cgram.c \
	    ${MIP}/xdefs.c ${MIP}/scan.c ${MIP}/pftn.c ${MIP}/trees.c \
	    ${MIP}/optim.c ${MIP}/reader.c ${MIP}/match.c ${MIP}/allo.c \
	    ${MIP}/common.c \
	    code.c local.c stab.c local2.c order.c table.c rel.c

tags:	${SRCS}
	ctags ${SRCS}

# XXX this is less than complete
depend: ${SRCS} ${MIP}/fort.c pcclocal.h pcctokens
	mkdep ${CFLAGS} ${INCS} ${SRCS} ${MIP}/fort.c
