# $Header: Makefile,v 1.6 87/05/08 17:09:50 ed Exp $
#
# $Log:	Makefile,v $
# Revision 1.6  87/05/08  17:09:50  ed
# Remove MFLAGS from clean and rcs targets.
# 
# Revision 1.5  87/01/05  12:05:38  ed
# Webster version -- redo make and make install
# 
# Revision 1.5  87/01/05  12:05:38  ed
# Updated to new directory structure
# 
# Revision 1.4  86/06/27  13:15:48  jqj
# added xnsbfs subdirectory, which contains a program to obtain CH addresses
# by local broadcast and print them.
# 
# Revision 1.3  86/06/25  13:02:19  jqj
# added ${CC} for subsidiary makes
# 
# Revision 1.2  85/10/21  12:44:23  root
# Gould version:  -g option breaks the Gould compiler, so build -lcourier
# without it.
# 
# Revision 1.1  85/10/18  14:24:35  root
# Initial revision
# 
# Revision 1.3  85/03/26  06:43:38  jqj
# Revised public alpha-test version, released 26 March 1985
# 
# Revision 1.2  85/03/26  06:11:47  jqj
# Revised public alpha-test version, released 26 March 1985
# 
# Revision 1.1  85/03/15  08:37:24  jqj
# Initial revision
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

CHDEFAULT = 

LIBDIR = ../lib
MANDIR = ../man
INCDIR = ../include
COURIERDIR = ../xnscourier

MFLAGS = DESTDIR=${DESTDIR} DESTBIN=${DESTBIN} DESTCOURIER=${DESTCOURIER} \
	CHDEFAULT="${CHDEFAULT}" \
	LIBDIR=../${LIBDIR} MANDIR=../${MANDIR} \
	INCDIR=../${INCDIR} COURIERDIR=../${COURIERDIR}


# xnsbfs must precede ch since xnsbfs is used to create clearinghouse.addresses
DIRS=xnsbfs ch auth
#CFLAGS= -g -DDEBUG
CFLAGS= -O
RCSREV=
RCSNAME=current

all:
	for Dir in ${DIRS}; do \
		(cd $$Dir; make ${MFLAGS} CC=${CC}); done

install:
	for Dir in ${DIRS}; do\
		(cd $$Dir; make ${MFLAGS} CC=${CC} install); done

clean:
	-rm -f *.BAK *.CKP
	for Dir in ${DIRS}; do\
		(cd $$Dir; make clean); done
	
rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
	for Dir in ${DIRS}; do\
		(cd $$Dir; make RCSNAME=${RCSNAME} RCSREV=${RCSREV} rcs); \
		done
