# $Header: Makefile,v 2.5 87/04/07 20:25:48 jqj Exp $
#CFLAGS=	-g -DDEBUG
CFLAGS= -O
LDFLAGS=-g
RCSREV=
ALL=	xnsftp 
BINDIR=../../bin
LIBDIR=../../lib

all: ${ALL}

xnsftp:	cmds.o cmdtab.o xnftp.o glob.o main.o errmsg.o Filing4_support.o Filing4_client.o
	${CC} ${LDFLAGS} cmds.o cmdtab.o xnftp.o glob.o main.o errmsg.o \
		Filing4_support.o Filing4_client.o ${LIBDIR}/libcourier.a \
		-o xnsftp

xnftp.o cmds.o cmdtab.o main.o errmsg.o: ftp_var.h Filing4.h

clean:
	-rm -f *.o *.BAK *.CKP Filing4_*.c Filing4_defs.h Filing4.h \
		xnsftp

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s xnsftp ${BINDIR}/xnsftp

Filing4_support.o Filing4_client.o Filing4_server.o Filing4_support.c Filing4_client.c Filing4_server.c: Filing4.h

Filing4_support.o: Filing4_support.c
	${CC} -c Filing4_support.c

Filing4.h: Filing4.cr
	${BINDIR}/xnscourier Filing4.cr
