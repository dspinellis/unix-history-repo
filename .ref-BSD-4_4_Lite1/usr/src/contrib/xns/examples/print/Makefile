# $Header: Makefile,v 1.4 87/03/03 06:09:45 jqj Exp $
#
# $Log:	Makefile,v $
# Revision 1.4  87/03/03  06:09:45  jqj
# deleted dependency on Printing3_defs.h
# 
# Revision 1.3  87/01/06  17:13:31  ed
# Webster version -- redo make and make install
# 
# Revision 1.3  87/01/06  17:13:31  ed
# Updated to new directory structure
# 
# Revision 1.2  86/06/25  12:51:23  jqj
# use ${CC} instead of cc
# 
# Revision 1.1  86/05/16  06:40:58  jqj
# Initial revision
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

BINDIR = ../../bin
LIBDIR = ../../lib
MANDIR = ../../man
INCDIR = ../../include
COURIERDIR = ../../xnscourier

CFLAGS=-O -I${INCDIR} -I../../courierlib

all: getprintstatus printtest xnsprint

install: all
	install -s -c getprintstatus ${BINDIR}
	install -s -c xnsprint ${BINDIR}

xnsprint: xnsprint.o
	${CC} -g -o xnsprint xnsprint.o ${LIBDIR}/libcourier.a

printtest: printtest.o
	${CC} -g -o printtest printtest.o  ${LIBDIR}/libcourier.a

getprintstatus: getprintstatus.o
	${CC} -g -o getprintstatus getprintstatus.o ${LIBDIR}/libcourier.a

Printing3_defs.h Printing3_support.c Printing3_client.c Printing3_server.c:
	${BINDIR}/xnscourier ${COURIERDIR}/Printing3.cr

#xnsprint.o getprintstatus.o Printing3_client.o Printing3_server.o: Printing3_defs.h

clean:
	-rm -f *.o *.BAK *.CKP xnsprint printtest getprintstatus \
		Printing3_defs.h \
		Printing3_server.c Printing3_client.c Printing3_support.c

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

