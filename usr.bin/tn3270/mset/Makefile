# $Header: /b/source/CVS/src/usr.bin/tn3270/mset/Makefile,v 1.5 1993/04/29 11:05:59 cgd Exp $

.include <../../Makefile.inc>

CFLAGS += -I${.CURDIR} -I.

MAN1 = mset.0
MAN5 = map3270.0

SRCS += astosc.c map3270.c mset.c

# this and the dependency hacks below to make 'depend' target
# work right...

DEPSRCS += astosc.OUT map3270.c mset.c

PROG = mset

astosc.o: astosc.OUT
CLEANFILES += astosc.OUT astosc.out
astosc.OUT: ${.CURDIR}/../ctlr/hostctlr.h ${.CURDIR}/../ctlr/function.h \
	${.CURDIR}/../ctlr/${KBD} ${.CURDIR}/../tools/mkastosc/${.CURDIR}/mkastosc
	${.CURDIR}/../tools/mkastosc/${.CURDIR}/mkastosc \
	${.CURDIR}/../ctlr/hostctlr.h ${.CURDIR}/../ctlr/function.h \
		< ${.CURDIR}/../ctlr/${KBD} > ${.TARGET}
	/bin/rm -f astosc.out
	ln -s astosc.OUT astosc.out

# astosc.out

${.CURDIR}/../tools/mkastosc/${.CURDIR}/mkastosc:
	cd ${.CURDIR}/../tools/mkastosc; make


depend: .depend
.depend: ${DEPSRCS}
	mkdep ${MKDEP} ${CFLAGS:M-[ID]*} ${.ALLSRC:M*.c}

.include <bsd.prog.mk>

.PATH: ${.CURDIR}/../api ${.CURDIR}/../ascii
