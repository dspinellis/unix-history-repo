# $Header: Makefile,v 1.1 87/03/23 12:42:01 ed Exp $
#
# Copyright (c) 1986, 1987 Xerox Corp.
#
# $Log:	Makefile,v $
# Revision 1.1  87/03/23  12:42:01  ed
# Initial revision
# 
# Revision 1.1  87/03/23  12:41:20  ed
# Initial revision
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

# directories
INCLUDE= ${INCDIR}/xnscourier

#CFLAGS= -O -g -I. -I${INCDIR} 
CFLAGS= -g -O -I${INCDIR}
LDFLAGS= -g
RCSREV=

ALL= xnscreds

OBJS= xnscreds.o 

xnscreds: ${OBJS}
	${CC} ${LDFLAGS} ${OBJS} ${LIBDIR}/libcourier.a \
		-o xnscreds

xnscreds.o: xnscreds.c ${INCLUDE}/Authentication2.h

clean:
	-rm -f *.o *.BAK *.CKP xnscreds.o ${ALL}

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s xnscreds ${BINDIR}/xnscreds

