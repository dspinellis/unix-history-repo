# $Header: Makefile,v 2.2 87/05/11 09:12:05 ed Exp $
# $Log:	Makefile,v $
# Revision 2.2  87/05/11  09:12:05  ed
# Added getXNSuser.
# 
# 
# Revision 2.1  87/01/27  07:13:27  jqj
# Webster version -- make versus make install
# (redid for new directory structure)
# 
DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

LIBDIR = ../../lib
MANDIR = ../../man
INCDIR = ../../include
COURIERDIR = ../../xnscourier

#CFLAGS=	-I. -I${INCDIR} -g -DDEBUG
CFLAGS= -O -I. -I${INCDIR}
OUT=${LIBDIR}/libcourier.a
OUT_P=${LIBDIR}/libcourier_p.a
SRCS= simpleauth.c getXNSpass.c getXNSuser.c Authcredcheck.c Authlookup.c
OBJS= simpleauth.o getXNSpass.o getXNSuser.o Authcredcheck.o Authlookup.o
RCSREV=
RCSNAME=current

.c.o:
	${CC} -c ${CFLAGS} -p $*.c
	mv $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c

install: ${OBJS}
	ar rv ${OUT} ${OBJS}
	cd profiled; ar rv ../${OUT_P} ${OBJS}
	ranlib ${OUT} ${OUT_P}

all:	${OBJS}

lint:	${SRCS}
	lint -hux ${SRCS}

tags:	${SRCS}
	ctags ${SRCS}

clean:
	-rm -f Authentication2_defs.h *.BAK *.CKP *.o profiled/*.o

Authlookup.o: Authlookup.c
	${CC} -c -p ${CFLAGS} -D'BFS="${DESTBIN}/xnsbfs -a"' Authlookup.c
	mv Authlookup.o profiled/Authlookup.o
	${CC} -c ${CFLAGS} -D'BFS="${DESTBIN}/xnsbfs -a"' Authlookup.c

Authcredcheck.o: Authcredcheck.c Authentication2_defs.h
	${CC} -c -p ${CFLAGS} Authcredcheck.c -I${INCDIR}/xnscourier
	mv Authcredcheck.o profiled/Authcredcheck.o
	${CC} -c ${CFLAGS} Authcredcheck.c -I${INCDIR}/xnscourier

Authentication2_defs.h: ../../courierlib/Authentication2_defs.h
	cp ../../courierlib/Authentication2_defs.h .

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

