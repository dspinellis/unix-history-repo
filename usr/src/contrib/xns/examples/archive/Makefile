# $Header: Makefile,v 1.1 87/03/17 16:27:31 ed Exp $
#
# Copyright (c) 1986, 1987 Xerox Corporation.
#
# $Log:	Makefile,v $
# Revision 1.1  87/03/17  16:27:31  ed
# Initial revision
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

#
#   XEROXFSCOMPATIBILITY	allows xnsftp use with Xerox File Servers until
#				Filingsubset is implemented
#   DEBUG			debug output generated
# 

# directories
INCLUDE= ${INCDIR}/xnscourier

#CFLAGS= -O -g -I${INCDIR} -DXEROXFSCOMPATIBILITY
CFLAGS= -g -O -I${INCDIR} -DXEROXFSCOMPATIBILITY
LDFLAGS= -g
RCSREV=

ALL= xnsarchive xnsrestore

ARCHIVEOBJS= xnsarchive.o

RESTOREOBJS= xnsrestore.o

all: ${ALL}

#
#  xnsarchive
#

xnsarchive:	${ARCHIVEOBJS}
	${CC} ${LDFLAGS} ${ARCHIVEOBJS} ${LIBDIR}/lib_filing.a \
		${LIBDIR}/libcourier.a ${LIBDIR}/lib_filing4.a \
		${LIBDIR}/libcourier.a \
		-o xnsarchive

xnsarchive.o : xnsarchive.c ${INCLUDE}/Filing4.h
	${CC} -c ${CFLAGS} xnsarchive.c

#
#  xnsarchive
#

xnsrestore:	${RESTOREOBJS}
	${CC} ${LDFLAGS} ${RESTOREOBJS} ${LIBDIR}/lib_filing.a \
		${LIBDIR}/libcourier.a ${LIBDIR}/lib_filing4.a \
		${LIBDIR}/libcourier.a \
		-o xnsrestore

xnsrestore.o : xnsrestore.c ${INCLUDE}/Filing4.h
	${CC} -c ${CFLAGS} xnsrestore.c

clean:
	-rm -f *.o *.BAK *.CKP ${ALL}

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s xnsarchive ${BINDIR}/xnsarchive
	install -c -s xnsrestore ${BINDIR}/xnsrestore

