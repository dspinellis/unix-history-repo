# $Header: Makefile,v 1.4 87/01/05 12:00:18 ed Exp $
# $Log:	Makefile,v $
# Revision 1.4  87/01/05  12:00:18  ed
# Webster version -- make versus make install
# 
# Revision 1.4  87/01/05  12:00:18  ed
# Updated to new directory structure
# 
# Revision 1.3  86/07/29  06:36:04  root
# add cleanup entry
# 
# Revision 1.2  86/06/29  07:12:54  jqj
# removed ns_ntoa(), since it's not a libc routine.
# 
# Revision 1.1  86/06/27  13:14:34  jqj
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

#CFLAGS=	-g -DDEBUG -I${INCDIR}
CFLAGS= -O -I${INCDIR}
RCSREV=
RCSNAME=current
SRCS= xnsbfs.c bfsgetdoms.c
OBJS= xnsbfs.o bfsgetdoms.o

xnsbfs: ${OBJS}
	${CC} ${LDFLAGS} -o xnsbfs ${OBJS} ${LIBDIR}/libcourier.a

xnsbfs.o: pex.h

install: xnsbfs
	install -s -c xnsbfs ${BINDIR}

clean:
	rm -f xnsbfs *.o *.BAK
	rm -f xnsbfs *.o

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
