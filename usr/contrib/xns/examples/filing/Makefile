#CFLAGS=	-g -DDEBUG
CFLAGS= -O
LDFLAGS=-g
RCSREV=
ALL=	xnsftp
BINDIR=/usr/new

all: ${ALL}

xnsftp:	cmds.o cmdtab.o xnftp.o glob.o main.o errmsg.o Filing4_support.o Filing4_client.o
	${CC} ${LDFLAGS} cmds.o cmdtab.o xnftp.o glob.o main.o errmsg.o \
		Filing4_support.o Filing4_client.o -lcourier -o xnsftp

xnftp.o cmds.o cmdtab.o main.o errmsg.o: ftp_var.h Filing4.h

clean:
	-rm -f *.o *.BAK *.CKP Filing4_*.c Filing4_defs.h Filing4.h \
		${ALL}

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: ${ALL}
	install -c -s xnsftp ${BINDIR}/xnsftp

Filing5_support.o Filing5_client.o Filing5_server.o Filing5_support.c Filing5_client.c Filing5_server.c: Filing5.h

Filing5.h: Filing5.cr
	${BINDIR}/xnscourier Filing5.cr

Filing4_support.o Filing4_client.o Filing4_server.o Filing4_support.c Filing4_client.c Filing4_server.c: Filing4.h

Filing4.h: Filing4.cr
	${BINDIR}/xnscourier Filing4.cr


