# $Header: Makefile,v 1.3 87/04/01 10:33:27 ed Exp $
#
# Copyright (c) 1986, 1987 Xerox Corporation.
#
# $Log:	Makefile,v $
# Revision 1.3  87/04/01  10:33:27  ed
# Change for new server libraries.
# 
# Revision 1.2  87/03/17  16:30:04  ed
# Cleaned up so all Filing servers are made in one directory.
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

#CFLAGS= -O -g -I${INCDIR} 
CFLAGS= -O -I${INCDIR}
LDFLAGS= -g
RCSREV=
ALL= Filing4d Filing5d Filing6d FilingSubset1d

all: ${ALL}

Filing4d: ${LIBDIR}/lib_filing4.a
	${CC} ${LDFLAGS} ../../courierlib/Filing4_s.o \
		${LIBDIR}/lib_filing4.a \
		${LIBDIR}/lib_filing.a ${LIBDIR}/libcourier.a \
		-o Filing4d

Filing5d: ${LIBDIR}/lib_filing5.a
	${CC} ${LDFLAGS} ../../courierlib/Filing5_s.o \
		${LIBDIR}/lib_filing5.a \
		${LIBDIR}/lib_filing.a ${LIBDIR}/libcourier.a \
		-o Filing5d

Filing6d: ${LIBDIR}/lib_filing6.a
	${CC} ${LDFLAGS} ../../courierlib/Filing6_s.o \
		${LIBDIR}/lib_filing6.a \
		${LIBDIR}/lib_filing.a ${LIBDIR}/libcourier.a \
		-o Filing6d

FilingSubset1d: ${LIBDIR}/lib_filingsubset1.a
	${CC} ${LDFLAGS} ../../courierlib/FSubset1_s.o \
		${LIBDIR}/lib_filingsubset1.a ${LIBDIR}/lib_filing.a \
		${LIBDIR}/lib_filingsubset1.a ${LIBDIR}/libcourier.a \
		-o FilingSubset1d

clean:
	-rm -f *.o *.BAK *.CKP ${ALL}

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s Filing4d ${COURIERDIR}/Filing4d
	install -c -s Filing5d ${COURIERDIR}/Filing5d
	install -c -s Filing6d ${COURIERDIR}/Filing6d
	install -c -s FilingSubset1d ${COURIERDIR}/FilingSubset1d

