# $Header: Makefile,v 2.2 87/01/31 12:55:03 jqj Exp $
#
# $Log:	Makefile,v $
# Revision 2.2  87/01/31  12:55:03  jqj
# make sure include/xnscourier directory exists
# 
# Revision 2.1  87/01/05  12:10:14  ed
# Webster version -- redo make and make install
# 
# Revision 2.1  87/01/05  12:10:14  ed
# Updated to new directory structure
# 
# Revision 2.0  85/11/21  07:22:01  jqj
# 4.3BSD standard release
# 
# Revision 1.3  85/11/20  12:28:16  jqj
# 4.3BSD standard release
# 
# Revision 1.2  85/10/21  12:45:32  jqj
# Gould version:  -g option breaks the compiler, so build -lcourier without
# it.
# 
# Revision 1.1  85/10/18  08:52:09  jqj
# Initial revision
# 
# Revision 1.5  85/03/26  06:08:22  jqj
# Revised public alpha-test version, released 26 March 1985
# 
# Revision 1.4  85/03/11  16:36:33  jqj
# Public alpha-test version, released 11 March 1985
# 
# Revision 1.3  85/03/11  16:34:14  jqj
# Public alpha-test version, released 11 March 1985
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

BINDIR = ../bin
LIBDIR = ../lib
MANDIR = ../man
INCDIR = ../include
COURIERDIR = ../xnscourier

MFLAGS = DESTDIR=${DESTDIR} DESTBIN=${DESTBIN} DESTCOURIER=${DESTCOURIER} \
	BINDIR=../${BINDIR} LIBDIR=../${LIBDIR} MANDIR=../${MANDIR} \
	INCDIR=../${INCDIR} COURIERDIR=../${COURIERDIR}

SRCS=	client.c server.c readwrite.c misc.c bdt.c names.c options.c \
		lookahead.c sppclose.c getcourierent.c getcservice.c except.c
OBJS=	client.o server.o readwrite.o misc.o bdt.o names.o options.o \
		lookahead.o sppclose.o getcourierent.o getcservice.o except.o
HDRS=	courier.h courierconnection.h courierdb.h except.h \
		realcourierconnection.h
#CFLAGS=	-g -DDEBUG -I.
CFLAGS= -O -I.
DESTDIR=
RCSREV=
RCSNAME=current
OUT = ${LIBDIR}/libcourier.a
OUT_P = ${LIBDIR}/libcourier_p.a

.c.o:
	${CC} -c ${CFLAGS} -p $*.c
	mv $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c

install: all
	-mkdir ${INCDIR}
	-mkdir ${INCDIR}/xnscourier
	install -c -m 644 courier.h ${INCDIR}/xnscourier
	install -c -m 644 courierdb.h ${INCDIR}/xnscourier
	install -c -m 644 realcourierconnection.h ${INCDIR}/xnscourier
	install -c -m 644 courierconnection.h ${INCDIR}/xnscourier
	install -c -m 644 except.h ${INCDIR}/xnscourier
	ar cr ${OUT} ${OBJS}
	cd profiled; ar cr ../${OUT_P} ${OBJS} 
	ranlib ${OUT} ${OUT_P}

all:	${OBJS}

getcourierent.o: courierdb.h getcourierent.c
	${CC} -c ${CFLAGS} -D'CSERVICES="${DESTCOURIER}/Courierservices"' \
		-p getcourierent.c
	mv getcourierent.o  profiled/getcourierent.o
	${CC} -c ${CFLAGS} -D'CSERVICES="${DESTCOURIER}/Courierservices"' \
		getcourierent.c

lookahead.o: courierdb.h lookahead.o
	${CC} -c ${CFLAGS} -D'COURLIB="${DESTCOURIER}"' -p lookahead.c
	mv lookahead.o profiled/lookahead.o
	${CC} -c ${CFLAGS} -D'COURLIB="${DESTCOURIER}"' lookahead.c

bdt.o client.o lookahead.o misc.o readwrite.o server.o: courier.h

bdt.o client.o lookahead.o readwrite.o server.o: realcourierconnection.h


lint:	${SRCS}
	lint -hux ${SRCS}

tags:	${SRCS} courier.h realcourierconnection.h
	ctags ${SRCS} courier.h realcourierconnection.h


clean:
	-rm -f ${OBJS} profiled/*.o *.BAK *.CKP

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
