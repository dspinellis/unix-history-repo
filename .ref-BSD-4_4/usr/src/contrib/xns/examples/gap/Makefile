# $Header: Makefile,v 2.3 87/05/08 16:46:21 ed Exp $
# $Log:	Makefile,v $
# Revision 2.3  87/05/08  16:46:21  ed
# Make use of -I switch on xnscourier.
# 
# Revision 2.2  87/01/06  16:38:28  ed
# Webster version -- redo make and make install
# 
# Revision 2.2  87/01/06  16:38:28  ed
# Updated to new directory structure
# 
# Revision 2.1  86/06/25  12:51:18  jqj
# use ${CC} instead of cc
# 
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
LDFLAGS=-g
RCSREV=

all: gaptelnet GAP3d

clean:
	-rm -f GAP3d gaptelnet *.o *.BAK *.CKP GAP3_*.c GAP3_defs.h GAP3.h \
		GAP2_*.c GAP2_*.h GAP2.h

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

install: GAP3d GAP3.h GAP2.h gaptelnet
	install -c -s GAP3d ${COURIERDIR}/GAP3d
	-install -c -s GAP2d ${COURIERDIR}/GAP2d
	install -c -m 644 GAP3.cr ${COURIERDIR}/GAP3.cr
	install -c -m 644 GAP3.h ${INCDIR}/xnscourier/GAP3.h
	install -c -m 644 GAP2.cr ${COURIERDIR}/GAP2.cr
	install -c -m 644 GAP2.h ${INCDIR}/xnscourier/GAP2.h
	install -c -s gaptelnet ${BINDIR}/gaptelnet

GAP3_support.o GAP3_client.o GAP3_serverx.o GAP3_support.c GAP3_client.c GAP3_server.c gaptelnet.o gaptelnetd.o: GAP3.h

GAP3.h: GAP3.cr
	${BINDIR}/xnscourier -I${COURIERDIR} GAP3.cr

GAP2_support.o GAP2_client.o GAP2_serverx.o GAP2_support.c GAP2_client.c GAP2_server.c gaptelnet.o gap2d.o: GAP2.h

GAP2.h: GAP2.cr
	${BINDIR}/xnscourier -I${COURIERDIR} GAP2.cr

gaptelnet: gaptelnet.o GAP3_support.o GAP3_client.o
	${CC} -o gaptelnet ${LDFLAGS} gaptelnet.o \
		GAP3_support.o GAP3_client.o ${LIBS} ${LIBDIR}/libcourier.a

gaptelnet.o: gaptelnet.c gapcontrols.h
	${CC} -c ${CFLAGS} gaptelnet.c

GAP3d: gaptelnetd.o GAP3_support.o GAP3_serverx.o
	${CC} -o GAP3d ${LDFLAGS} gaptelnetd.o \
		GAP3_support.o GAP3_serverx.o ${LIBDIR}/libcourier.a

# have to split out Server() since it's special here
# routine Server() appears in gaptelnetd.c
GAP3_serverx.o: GAP3_server.c
	sed -e '/^Server(/,/^}/d' GAP3_server.c > GAP3_serverx.c
	${CC} ${CFLAGS} -c GAP3_serverx.c


GAP2d: gap2d.o GAP2_support.o GAP2_serverx.o gap2d-unimp.o
	${CC} -o GAP2d ${LDFLAGS} gap2d.o gap2d-unimp.o \
		GAP2_support.o GAP2_serverx.o ${LIBDIR}/libcourier.a

# have to split out Server() since it's special here
# routine Server() appears in gaptelnetd.c
GAP2_serverx.o: GAP2_server.c
	sed -e '/^Server(/,/^}/d' GAP2_server.c > GAP2_serverx.c
	${CC} ${CFLAGS} -c GAP2_serverx.c
