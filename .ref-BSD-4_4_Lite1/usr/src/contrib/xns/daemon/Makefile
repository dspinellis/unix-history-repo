# $Header: Makefile,v 2.2 87/02/14 13:49:27 jqj Exp $
#
# $Log:	Makefile,v $
# Revision 2.2  87/02/14  13:49:27  jqj
# new directory structure
# 
# Revision 2.1  87/01/15  08:53:39  ed
# Updated to new directory structure
# 
# Revision 2.0  85/11/21  07:21:55  jqj
# 4.3BSD standard release
# 
# Revision 1.2  85/10/30  07:39:52  root
# default CRLIB
# 
# Revision 1.1  85/10/30  07:38:14  root
# Initial revision
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

LIBDIR = ../lib
MANDIR = ../man
INCDIR = ../include
COURIERDIR = ../xnscourier

SRCS= xnscourierd.c
OBJS= xnscourierd.o
#CFLAGS=	-g -DDEBUG
CFLAGS= -O -I${INCDIR}
RCSREV=
RCSNAME=current

xnscourierd: xnscourierd.o
	${CC} ${CFLAGS} -o xnscourierd xnscourierd.o ${LIBDIR}/libcourier.a

lint:	${SRCS}
	lint -hux ${SRCS}

install: xnscourierd
	install -s -c xnscourierd ${COURIERDIR}/xnscourierd

clean:
	-rm -f ${OBJS} *.BAK *.CKP xnscourierd *.o

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
