# $Header: Makefile,v 2.0 85/11/21 07:22:01 jqj Exp $
#
# $Log:	Makefile,v $
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
INCLUDE = ${DESTDIR}/usr/include/xnscourier
OUT = ${DESTDIR}/usr/local/lib/libcourier.a
OUT_P = ${DESTDIR}/usr/local/lib/libcourier_p.a
CRLIB = ${DESTDIR}/usr/new/lib/xnscourier

.c.o:
	${CC} -c ${CFLAGS} -p $*.c
	mv $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c

install: all
	-mkdir ${INCLUDE}
	install -c -m 644 courier.h ${INCLUDE}
	install -c -m 644 courierdb.h ${INCLUDE}
	install -c -m 644 realcourierconnection.h ${INCLUDE}
	install -c -m 644 courierconnection.h ${INCLUDE}
	install -c -m 644 except.h ${INCLUDE}
	ar cr ${OUT} ${OBJS}
	cd profiled; ar cr ${OUT_P} ${OBJS} 
	ranlib ${OUT} ${OUT_P}

all:	${OBJS}

getcourierent.o: courierdb.h getcourierent.c
	${CC} -c ${CFLAGS} -D'CSERVICES="${CRLIB}/Courierservices"' \
		-p getcourierent.c
	mv getcourierent.o  profiled/getcourierent.o
	${CC} -c ${CFLAGS} -D'CSERVICES="${CRLIB}/Courierservices"' \
		getcourierent.c

lookahead.o: courierdb.h lookahead.o
	${CC} -c ${CFLAGS} -D'COURLIB="${CRLIB}"' -p lookahead.c
	mv lookahead.o profiled/lookahead.o
	${CC} -c ${CFLAGS} -D'COURLIB="${CRLIB}"' lookahead.c

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
