# $Header: Makefile,v 1.1 87/03/17 16:28:58 ed Exp $
#
# Copyright (c) 1986, 1987 Xerox Corporation.
#
# $Log:	Makefile,v $
# Revision 1.1  87/03/17  16:28:58  ed
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

ALL= xnsbrowse xnsrprint

BROWSEOBJS= xnsbrowse.o 

RPRINTOBJS= xnsrprint.o


all: ${ALL}

#
#  xnsbrowse
#

xnsbrowse:	${BROWSEOBJS}
	${CC} ${LDFLAGS} ${BROWSEOBJS} ${LIBDIR}/lib_filing.a \
		${LIBDIR}/libcourier.a ${LIBDIR}/lib_filing4.a \
		${LIBDIR}/libcourier.a \
		-o xnsbrowse

xnsbrowse.o : xnsremote.c ${INCLUDE}/FilingSubset1.h
	${CC} -c ${CFLAGS} -o xnsbrowse.o xnsremote.c

#
#  xnsrprint
#

xnsrprint:	${RPRINTOBJS}
	${CC} ${LDFLAGS} ${RPRINTOBJS} ${LIBDIR}/lib_filing.a \
		${LIBDIR}/libcourier.a ${LIBDIR}/lib_filing4.a \
		${LIBDIR}/libcourier.a \
		-o xnsrprint

xnsrprint.o : xnsremote.c ${INCLUDE}/FilingSubset1.h
	${CC} -c ${CFLAGS} -DXNSPRINT='"${DESTBIN}/xnsprint"' \
			-DMAHA='"${DESTBIN}/maha"' -DPRINTOPTION \
			-o xnsrprint.o xnsremote.c

clean:
	-rm -f *.o *.BAK *.CKP ${ALL}

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s xnsbrowse ${BINDIR}/xnsbrowse
	install -c -s xnsrprint ${BINDIR}/xnsrprint

