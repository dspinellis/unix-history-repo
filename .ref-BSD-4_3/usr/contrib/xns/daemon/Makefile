# $Header: Makefile,v 2.0 85/11/21 07:21:55 jqj Exp $
#
# $Log:	Makefile,v $
# Revision 2.0  85/11/21  07:21:55  jqj
# 4.3BSD standard release
# 
# Revision 1.2  85/10/30  07:39:52  root
# default CRLIB
# 
# Revision 1.1  85/10/30  07:38:14  root
# Initial revision
# 

SRCS= xnscourierd.c
OBJS= xnscourierd.o
#CFLAGS=	-g -DDEBUG
CFLAGS= -O
DESTDIR=
RCSREV=
RCSNAME=current
CRLIB=/usr/new/lib/xnscourier

xnscourierd: xnscourierd.o
	${CC} ${CFLAGS} -o xnscourierd xnscourierd.o -lcourier

lint:	${SRCS}
	lint -hux ${SRCS}

install: xnscourierd
	install -s -c xnscourierd ${DESTDIR}${CRLIB}/xnscourierd
	@echo "Please add ${CRLIB}/xnscourierd to your /etc/rc.local file"

clean:
	-rm -f ${OBJS} *.BAK *.CKP xnscourierd *.o

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
