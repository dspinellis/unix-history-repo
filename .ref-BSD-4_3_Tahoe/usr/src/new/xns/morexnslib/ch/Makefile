# $Header: Makefile,v 2.6 87/05/11 16:38:05 ed Exp $
#
# $Log:	Makefile,v $
# Revision 2.6  87/05/11  16:38:05  ed
# Added CHretrieve.c
# 
# Revision 2.5  87/01/27  08:04:34  jqj
# if xnsbfs fails to find a CHS, then bomb
# 
# Revision 2.4  87/01/27  07:15:12  jqj
# Webster changes for new directory structure -- make versus make install
# 
DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

CHDEFAULT = 

BINDIR = ../../bin
LIBDIR = ../../lib
MANDIR = ../../man
INCDIR = ../../include
COURIERDIR = ../../xnscourier

#CFLAGS=	-g -DDEBUG -I${INCDIR}
CFLAGS= -O -I${INCDIR}
OUT=${LIBDIR}/libcourier.a
OUT_P=${LIBDIR}/libcourier_p.a
SRCS= CHlookup.c CHenumerate.c CHenumalias.c CHstrtoname.c CHdefault.c \
	CHusername.c CHretrieve.c
OBJS= CHlookup.o CHenumerate.o CHenumalias.o CHstrtoname.o CHdefault.o \
	CHusername.o CHretrieve.o
RCSREV=
RCSNAME=current

.c.o:
	${CC} -c ${CFLAGS} -p $*.c -I${INCDIR}/xnscourier
	mv $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c -I${INCDIR}/xnscourier

install: ${OBJS} CH.default clearinghouse.addresses
	ar rv ${OUT} ${OBJS}
	cd profiled; ar rv ../${OUT_P} ${OBJS}
	ranlib ${OUT} ${OUT_P}
	rm -rf ${INCDIR}/xnscourier/CH.h \
		${COURIERDIR}/CH.addrs  ${COURIERDIR}/CH.default
	cp CH.h ${INCDIR}/xnscourier	
	cp clearinghouse.addresses ${COURIERDIR}/CH.addrs
	cp CH.default ${COURIERDIR}
	chmod 644 ${INCDIR}/xnscourier/CH.h \
		${COURIERDIR}/CH.addrs ${COURIERDIR}/CH.default

all:	${OBJS} ${TESTS}

lint:	${SRCS}
	lint -hux ${SRCS}

tags:	${SRCS}
	ctags ${SRCS}

clean:
	-rm -f CH.default clearinghouse.addresses \
		Clearinghouse2.h Clearinghouse2_defs.h \
		*.BAK *.CKP *.o profiled/*.o

CHlookup.o CHdefault.o CHenumerate.o CHenumalias.o: Clearinghouse2_defs.h

CHlookup.o: CHlookup.c
	${CC} -c -p ${CFLAGS} -D'BFS="${DESTBIN}/xnsbfs"' \
		CHlookup.c
	mv CHlookup.o profiled/CHlookup.o
	${CC} -c ${CFLAGS} -D'BFS="${DESTBIN}/xnsbfs"' \
		CHlookup.c

CHdefault.o: CHdefault.c
	${CC} -c -p ${CFLAGS} \
		-D'CHADDRS="${DESTCOURIER}/CH.addrs"' \
		-D'CHDEFPATH="${DESTCOURIER}/CH.default"' \
		CHdefault.c
	mv CHdefault.o profiled/CHdefault.o
	${CC} -c ${CFLAGS} \
		-D'CHADDRS="${DESTCOURIER}/CH.addrs"' \
		-D'CHDEFPATH="${DESTCOURIER}/CH.default"' \
		CHdefault.c

Clearinghouse2_defs.h: ../../courierlib/Clearinghouse2_defs.h
	cp ../../courierlib/Clearinghouse2_defs.h .

CH.default:
	@echo if \"${CHDEFAULT}\" is not the local domain:organization
	@echo edit Makefile to change CHDEFAULT and rerun make
	echo "${CHDEFAULT}" | cat CH.stencil - > CH.default

clearinghouse.addresses:
	${BINDIR}/xnsbfs > clearinghouse.addresses
	(test -s clearinghouse.addresses || \
		echo ERROR: no local CH found. Edit clearinghouse.addresses and rerun make)
	test -s clearinghouse.addresses

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
