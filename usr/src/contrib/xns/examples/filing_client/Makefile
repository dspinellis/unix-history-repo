# $Header: Makefile,v 1.2 87/04/01 13:51:36 ed Exp $
#
# $Log:	Makefile,v $
# Revision 1.2  87/04/01  13:51:36  ed
# Redone for new directory structure.
# 
# Revision 1.1  87/01/14  13:06:19  ed
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
# compile flags
#   XEROXFSCOMPATIBILITY	allows xnsftp use with Xerox File Servers until
#				Filingsubset is implemented
#   DEBUG			debug output generated
# 

# directories
INCLUDE= ${INCDIR}/xnscourier

#CFLAGS= -O -g -I${INCDIR} -DXEROXFSCOMPATIBILITY -DDEBUG
CFLAGS= -g -O -I${INCDIR} -DXEROXFSCOMPATIBILITY
LDFLAGS= -g
RCSREV=

ALL= xnsftp

OBJS= cmds.o cmdtab.o xnftp.o glob.o main.o 

all: ${ALL}

#
#  client
#

xnsftp:	${OBJS}
	${CC} ${LDFLAGS} ${OBJS} ${LIBDIR}/lib_filing.a ${LIBDIR}/libcourier.a \
		${LIBDIR}/lib_filing4.a ${LIBDIR}/libcourier.a \
		-o xnsftp

xnftp.o attributes.o cmds.o cmdtab.o main.o errmsg.o: ftp_var.h  \
		${INCLUDE}/FilingSubset1.h

clean:
	-rm -f *.o *.BAK *.CKP ${ALL}

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s xnsftp ${BINDIR}/xnsftp

