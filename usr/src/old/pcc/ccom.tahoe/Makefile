#
#	%MIP%	1.6	87/06/06
#
# Makefile for tahoe pcc
#
RM=	/bin/rm -f
GREP=	egrep
MIP=	../../mip
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
INCS=	-I. -I${MIP}
CFLAGS=	-O
LDFLAGS=

# files common to single and double precision compilers
SRCS=	code.c local.c local2.c order.c rel.c stab.c \
	${MIP}/common.c ${MIP}/match.c ${MIP}/optim.c ${MIP}/pftn.c \
	${MIP}/reader.c ${MIP}/scan.c ${MIP}/xdefs.c ${MIP}/cgram.y
OBJS=	code.o local.o local2.o order.o rel.o stab.o \
	comm1.o match.o optim.o pftn.o reader.o scan.o xdefs.o cgram.o \
	rodata.o
# double precision files
DOPTS=	-DONEPASS ${INCS} ${CONFIG}
DFLAGS=	${CFLAGS} ${DOPTS}
DSRCS=	${MIP}/allo.c ${MIP}/trees.c table.c
DOBJS=	allo.o trees.o table.o
# single precision stuff
SOPTS=	-DSPRECC -DONEPASS ${INCS} ${CONFIG}
SFLAGS=	${CFLAGS} ${SOPTS}
SSRCS=	strees.c sallo.c stable.c
SOBJS=	strees.o sallo.o stable.o
# fortran version
FOPTS=	-DFORT ${INCS} ${CONFIG}
FFLAGS=	${CFLAGS} ${FOPTS}
FSRCS=	freader.c fallo.c fmatch.c ftable.c forder.c flocal2.c fcomm2.c ftable.c
FOBJS=	freader.o fallo.o fmatch.o ftable.o forder.o flocal2.o fcomm2.o ftable.o

TESTDIR=.

all:	ccom sccom f1

#
# 'ccom' is a one-pass C compiler.
#
ccom:	${OBJS} ${DOBJS} rel.o
	${CC} ${LDFLAGS} -o ${TESTDIR}/ccom ${OBJS} ${DOBJS} rel.o
trees.o: ${MIP}/trees.c
	${CC} -c ${DFLAGS} ${MIP}/trees.c
optim.o: ${MIP}/optim.c
	${CC} -c ${DFLAGS} ${MIP}/optim.c
pftn.o: ${MIP}/pftn.c
	${CC} -c ${DFLAGS} ${MIP}/pftn.c
code.o: code.c
	${CC} -c ${DFLAGS} code.c
local.o: local.c
	${CC} -c ${DFLAGS} local.c
scan.o: ${MIP}/scan.c
	${CC} -c ${DFLAGS} ${MIP}/scan.c
xdefs.o: ${MIP}/xdefs.c
	${CC} -c ${DFLAGS} ${MIP}/xdefs.c
cgram.o: cgram.c
	${CC} -c ${DFLAGS} cgram.c
rodata.o: rodata.c
	${CC} -c ${DFLAGS} -R rodata.c
rodata.c cgram.c: ${MIP}/cgram.y ./pcctokens
	cat pcctokens ${MIP}/cgram.y > gram.in
	${YACC} gram.in
	${RM} rodata.c
	sh ./:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c cgram.c
comm1.o: ${MIP}/common.c
	ln ${MIP}/common.c comm1.c
	${CC} -c ${DFLAGS} -DPASS1COMMON comm1.c
	${RM} comm1.c
stab.o: stab.c
	${CC} -c ${DFLAGS} stab.c
table.o: table.c
	${CC} -c ${DFLAGS} -R table.c
reader.o: ${MIP}/reader.c
	${CC} -c ${DFLAGS} ${MIP}/reader.c
local2.o: local2.c
	${CC} -c ${DFLAGS} local2.c
order.o: order.c
	${CC} -c ${DFLAGS} order.c
match.o: ${MIP}/match.c
	${CC} -c ${DFLAGS} ${MIP}/match.c
allo.o: ${MIP}/allo.c
	${CC} -c ${DFLAGS} ${MIP}/allo.c

#
# 'sccom' is a C compiler that does single precision computations in
#	single precision rather than double precision.
#
sccom:	${OBJS} ${SOBJS} rel.o
	${CC} ${LDFLAGS} -o ${TESTDIR}/sccom ${OBJS} ${SOBJS} rel.o
strees.o: ${MIP}/trees.c
	ln ${MIP}/trees.c strees.c
	${CC} -c ${SFLAGS} strees.c
	${RM} strees.c
sallo.o: ${MIP}/allo.c
	ln ${MIP}/allo.c sallo.c
	${CC} -c ${SFLAGS} sallo.c
	${RM} sallo.c
stable.o: table.c
	ln table.c stable.c
	${CC} -c ${SFLAGS} -R stable.c
	${RM} stable.c

#
# 'f1' is the f77 and pc code generator.
#
f1:	fort.o ${FOBJS}
	${CC} ${LDFLAGS} -o ${TESTDIR}/f1 fort.o ${FOBJS}
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

install: all
	install -s ${TESTDIR}/ccom ${DESTDIR}/lib/ccom
	install -s ${TESTDIR}/sccom ${DESTDIR}/lib/sccom
	install -s ${TESTDIR}/f1 ${DESTDIR}/lib/f1

./pcclocal.h: localdefs.h /usr/include/pcc.h
	${RM} pcclocal.h
	cat /usr/include/pcc.h localdefs.h | \
	    ${GREP} '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	    sed -e 's/PCC[A-Z]*_//' > pcclocal.h 

./pcctokens: localdefs.h /usr/include/pcc.h
	${RM} pcctokens
	cat /usr/include/pcc.h localdefs.h | \
	    ${GREP} '^#[ 	]*define[ 	][ 	]*PCC_' | sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > pcctokens

clean:
	${RM} *.o ccom sccom f1 cgram.c rodata.c pcctokens pcclocal.h \
	    gram.in ${SSRCS} ${FSRCS}

lint:
	lint -hax ${INCS} -I${MIP} cgram.c \
	    ${MIP}/xdefs.c ${MIP}/scan.c ${MIP}/pftn.c ${MIP}/trees.c \
	    ${MIP}/optim.c ${MIP}/reader.c ${MIP}/match.c ${MIP}/allo.c \
	    ${MIP}/comm1.c \
	    code.c local.c stab.c local2.c order.c table.c

tags:	${SRCS} ${DSRCS}
	ctags ${SRCS} ${DSRCS}

# XXX this is less than complete
depend: ${SRCS} ${DSRCS} ./pcclocal.h ./pcctokens
	for i in ${SRCS} ${DSRCS}; do \
	    cc -M ${DOPTS} $$i | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE

code.o: code.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
code.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
code.o: ../../mip/onepass.h ../../mip/ndu.h /usr/include/sys/types.h
code.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
local.o: local.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
local.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
local.o: ../../mip/onepass.h ../../mip/ndu.h
local2.o: local2.c ../../mip/pass2.h ./macdefs.h ./mac2defs.h
local2.o: ../../mip/manifest.h /usr/include/stdio.h ./pcclocal.h
local2.o: ../../mip/config.h ../../mip/onepass.h ../../mip/ndu.h
local2.o: /usr/include/ctype.h
order.o: order.c ../../mip/pass2.h ./macdefs.h ./mac2defs.h
order.o: ../../mip/manifest.h /usr/include/stdio.h ./pcclocal.h
order.o: ../../mip/config.h ../../mip/onepass.h ../../mip/ndu.h
rel.o: rel.c
stab.o: stab.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
stab.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
stab.o: ../../mip/onepass.h ../../mip/ndu.h /usr/include/sys/types.h
stab.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
common.o: ../../mip/common.c
match.o: ../../mip/match.c ../../mip/pass2.h ./macdefs.h ./mac2defs.h
match.o: ../../mip/manifest.h /usr/include/stdio.h ./pcclocal.h
match.o: ../../mip/config.h ../../mip/onepass.h ../../mip/ndu.h
optim.o: ../../mip/optim.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
optim.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
optim.o: ../../mip/onepass.h ../../mip/ndu.h
pftn.o: ../../mip/pftn.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
pftn.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
pftn.o: ../../mip/onepass.h ../../mip/ndu.h
reader.o: ../../mip/reader.c ../../mip/pass2.h ./macdefs.h ./mac2defs.h
reader.o: ../../mip/manifest.h /usr/include/stdio.h ./pcclocal.h
reader.o: ../../mip/config.h ../../mip/onepass.h ../../mip/ndu.h
scan.o: ../../mip/scan.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
scan.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
scan.o: ../../mip/onepass.h ../../mip/ndu.h /usr/include/a.out.h
scan.o: /usr/include/sys/exec.h /usr/include/stab.h /usr/include/ctype.h
scan.o: /usr/include/signal.h
xdefs.o: ../../mip/xdefs.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
xdefs.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
xdefs.o: ../../mip/onepass.h ../../mip/ndu.h
cgram.o: ../../mip/cgram.y ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
cgram.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
cgram.o: ../../mip/onepass.h ../../mip/ndu.h
allo.o: ../../mip/allo.c ../../mip/pass2.h ./macdefs.h ./mac2defs.h
allo.o: ../../mip/manifest.h /usr/include/stdio.h ./pcclocal.h
allo.o: ../../mip/config.h ../../mip/onepass.h ../../mip/ndu.h
trees.o: ../../mip/trees.c ../../mip/pass1.h ./macdefs.h ../../mip/manifest.h
trees.o: /usr/include/stdio.h ./pcclocal.h ../../mip/config.h
trees.o: ../../mip/onepass.h ../../mip/ndu.h /usr/include/setjmp.h
table.o: table.c ../../mip/pass2.h ./macdefs.h ./mac2defs.h
table.o: ../../mip/manifest.h /usr/include/stdio.h ./pcclocal.h
table.o: ../../mip/config.h ../../mip/onepass.h ../../mip/ndu.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
