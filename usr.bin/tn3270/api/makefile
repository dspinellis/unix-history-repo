#	@(#)makefile	4.5 (Berkeley) 4/26/91
#
#  PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
#  --------------------         -----   ----------------------
#  CURRENT PATCH LEVEL:         1       00084
#  --------------------         -----   ----------------------
# 
#  28 Feb 93    Nick Handel	Use correct mk template file.
#
DEBUG_FLAGS = #-g

CFLAGS = ${DEBUG_FLAGS}
#CFLAGS = -I obj ${DEBUG_FLAGS}

PRINT = lpr -p

KBD	= 3270pc.kbd
KBD	= unix.kbd

SRCS = apilib.c api_bsd.c api_exch.c asc_ebc.c astosc.c dctype.c \
		disp_asc.c ebc_disp.c

ALLH = apilib.h api_exch.h asc_ebc.h astosc.h disp_asc.h dctype.h ebc_disp.h


ALLHC=	${ALLH} ${SRCS}
ALLPRINT =	${ALLHC}

ALLSOURCE = ${ALLPRINT} makefile makefile.mak

CLEANFILES =	disp_out asc_disp.out astosc.out disp_asc.out \
		astosc.OUT disp_asc.OUT \
		test* test.o t1* t1.o t2* t2.o

#LIBDIR	= ${.CURDIR}/obj
#LINTLIBDIR	= ${.CURDIR}/obj
LIB	= api

.DEFAULT:
	sccs get $<

sccsclean:
	-sccs clean
	-sccs get makefile

action:
	${ACTION}

test:	apilib.a test.o
	${CC} ${CFLAGS} -o $@ test.o apilib.a

t1:	apilib.a t1.o
	${CC} ${CFLAGS} -o $@ t1.o apilib.a

t2:	apilib.a t2.o
	${CC} ${CFLAGS} -o $@ t2.o apilib.a

print:
	${PRINT} ${ALLPRINT}

clist:	${SRCS}
	@for i in ${SRCS} ; \
		do (echo ${DIRPATH}$$i); done

hclist:	${ALLHC}
	@for i in ${ALLHC} ; \
		do (echo ${DIRPATH}$$i); done

sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done

astosc.OUT:	../ctlr/function.h ../ctlr/hostctlr.h ../ctlr/$(KBD)
	(cd ${.CURDIR}/../tools; make mkastosc )
	-rm $@ astosc.out
	../tools/mkastosc ${.CURDIR}/../ctlr/hostctlr.h \
		${.CURDIR}/../ctlr/function.h < \
				${.CURDIR}/../ctlr/$(KBD) > $@
	ln $@ astosc.out

asc_disp.out:	ebc_disp.o
	(cd ${.CURDIR}/../tools; make mkastods )
	-rm $@
	../tools/mkastods > $@

disp_asc.OUT:	ebc_disp.o
	(cd ${.CURDIR}/../tools; make mkdstoas )
	-rm $@ disp_asc.out
	../tools/mkdstoas > $@
	ln $@ disp_asc.out

# Some special dependencies...
astosc.o:	astosc.OUT
disp_asc.o:	disp_asc.OUT asc_disp.out

.MAIN:	myall
# Actually do some work
.include <bsd.lib.mk>

myall:	lib${LIB}.a
	ranlib lib${LIB}.a
