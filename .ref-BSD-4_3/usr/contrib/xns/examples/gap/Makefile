# $Header: Makefile,v 2.0 85/11/21 07:22:57 jqj Exp $
# $Log:	Makefile,v $
# Revision 2.0  85/11/21  07:22:57  jqj
# 4.3BSD standard release
# 
# Revision 1.2  85/10/21  12:32:46  root
# Changed for Gould
# 
# Revision 1.1  85/10/18  09:41:38  root
# Initial revision
# 
# Revision 1.1  85/05/22  09:45:36  jqj
# Initial revision
# 

#CFLAGS=	-g -DDEBUG
CFLAGS= -O
LDFLAGS=-g
INCLUDE=${DESTDIR}/usr/include/xnscourier
RCSREV=
BINDIR=/usr/new
CRLIB=${DESTDIR}/usr/new/lib/xnscourier

all: gaptelnet GAP3d GAP2d

clean:
	-rm -f gaptelnet *.o *.BAK *.CKP GAP3_*.c GAP3_defs.h GAP3.h \
		GAP2_*.c GAP2_*.h GAP2.h GAP2d GAP3d

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: GAP3d GAP3.h GAP2.h gaptelnet
	install -c -s GAP3d ${CRLIB}/GAP3d
	-install -c -s GAP2d ${CRLIB}/GAP2d
	install -c -m 644 GAP3.cr ${CRLIB}/GAP3.cr
	install -c -m 644 GAP3.h ${INCLUDE}/GAP3.h
	install -c -m 644 GAP2.cr ${CRLIB}/GAP2.cr
	install -c -m 644 GAP2.h ${INCLUDE}/GAP2.h
	install -c -s gaptelnet ${BINDIR}/gaptelnet

GAP3_support.o GAP3_client.o GAP3_serverx.o GAP3_support.c GAP3_client.c GAP3_server.c gaptelnet.o gaptelnetd.o: GAP3.h

GAP3.h: GAP3.cr
	${BINDIR}/xnscourier GAP3.cr

GAP2_support.o GAP2_client.o GAP2_serverx.o GAP2_support.c GAP2_client.c GAP2_server.c gaptelnet.o gap2d.o: GAP2.h

GAP2.h: GAP2.cr
	${BINDIR}/xnscourier GAP2.cr

gaptelnet: gaptelnet.o GAP3_support.o GAP3_client.o
	cc -o gaptelnet ${LDFLAGS} gaptelnet.o \
		GAP3_support.o GAP3_client.o ${LIBS} -lcourier

gaptelnet.o: gaptelnet.c gapcontrols.h
	${CC} -c ${CFLAGS} gaptelnet.c

GAP3d: gaptelnetd.o GAP3_support.o GAP3_serverx.o
	cc -o GAP3d ${LDLAGS} gaptelnetd.o \
		GAP3_support.o GAP3_serverx.o -lcourier

# have to split out Server() since it's special here
# routine Server() appears in gaptelnetd.c
GAP3_serverx.o: GAP3_server.c
	sed -e '/^Server(/,/^}/d' GAP3_server.c > GAP3_serverx.c
	${CC} ${CFLAGS} -c GAP3_serverx.c


GAP2d: gap2d.o GAP2_support.o GAP2_serverx.o gap2d-unimp.o
	cc -o GAP2d ${LDLAGS} gap2d.o gap2d-unimp.o \
		GAP2_support.o GAP2_serverx.o -lcourier

# have to split out Server() since it's special here
# routine Server() appears in gaptelnetd.c
GAP2_serverx.o: GAP2_server.c
	sed -e '/^Server(/,/^}/d' GAP2_server.c > GAP2_serverx.c
	${CC} ${CFLAGS} -c GAP2_serverx.c
