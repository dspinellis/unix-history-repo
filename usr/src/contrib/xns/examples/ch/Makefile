# $Header: Makefile,v 1.4 87/01/27 07:11:27 jqj Exp $
#
# $Log:	Makefile,v $
# Revision 1.4  87/01/27  07:11:27  jqj
# Webster version -- make versus make install
# 
# Revision 1.2  87/01/06  17:19:29  ed
# Updated to new directory structure
# 
# 
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

BINDIR = ../../bin
LIBDIR = ../../lib
MANDIR = ../../man
INCDIR = ../../include
COURIERDIR = ../../xnscourier

#CFLAGS=	-g -DDEBUG -I${INCDIR}
CFLAGS= -O -I${INCDIR}
RCSREV=
RCSNAME=current
TESTS=enumerate listaddr listaliasof listproperties registerhost

all: ${TESTS}

clean:
	-rm -f ${TESTS} *.o *.BAK *.CKP Cl*.[ch]

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: all
	install -s -c registerhost ${BINDIR}

# test programs

registerhost.c enumerate.c listaddr.c listaliasof.c listproperties.c: Clearinghouse2_defs.h

Clearinghouse2_defs.h: ../../courierlib/Clearinghouse2_defs.h
	cp ../../courierlib/Clearinghouse2_defs.h .
	cp ../../courierlib/Clearinghouse2.h .

enumerate: enumerate.o
	${CC} -g -o enumerate enumerate.o ${LIBDIR}/libcourier.a

listaddr: listaddr.o
	${CC} -g -o listaddr listaddr.o ${LIBDIR}/libcourier.a

listaliasof: listaliasof.o
	${CC} -g -o listaliasof listaliasof.o  ${LIBDIR}/libcourier.a

listproperties: listproperties.o
	${CC} -g -o listproperties listproperties.o  ${LIBDIR}/libcourier.a

registerhost: registerhost.o
	${CC} -g -o registerhost registerhost.o ${LIBDIR}/libcourier.a

