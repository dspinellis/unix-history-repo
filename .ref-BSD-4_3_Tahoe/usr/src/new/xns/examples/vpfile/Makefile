# $Header: Makefile,v 1.1 87/03/17 16:28:34 ed Exp $
#
# Copyright (c) 1986, 1987 Xerox Corporation.
#
# $Log:	Makefile,v $
# Revision 1.1  87/03/17  16:28:34  ed
# Initial revision
# 
# 
# 
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new

BINDIR = ../../bin
LIBDIR = ../../lib
MANDIR = ../../man
INCDIR = ../../include
COURIERDIR = ../../xnscourier


#CFLAGS= -O -g -I${INCDIR} -DXEROXFSCOMPATIBILITY
CFLAGS= -g -O -I${INCDIR} -DXEROXFSCOMPATIBILITY
LDFLAGS= -g
RCSREV=

ALL= vpfile

OBJS= vpfile.o

all: ${ALL}

vpfile:	${OBJS}
	${CC} ${LDFLAGS} ${OBJS} ${LIBDIR}/lib_filing.a \
		${LIBDIR}/libcourier.a ${LIBDIR}/lib_filing4.a \
		${LIBDIR}/libcourier.a \
		-o vpfile

clean:
	-rm -f *.o *.BAK *.CKP ${ALL}

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s vpfile ${BINDIR}/vpfile

