# $Header: Makefile,v 1.2 85/10/21 12:44:23 root Exp $
#
# $Log:	Makefile,v $
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

DIRS=ch auth
CFLAGS=	-g -DDEBUG
#CFLAGS= -O
DESTDIR=
RCSREV=
RCSNAME=current
CRLIB=${DESTDIR}/usr/new/lib/xnscourier
BINDIR=${DESTDIR}/usr/new

all:
	for Dir in ${DIRS}; do \
		(cd $$Dir; make CRLIB=${CRLIB} BINDIR=${BINDIR}); done

install:
	for Dir in ${DIRS}; do\
		(cd $$Dir; make DESTDIR=${DESTDIR} CRLIB=${CRLIB} \
			BINDIR=${BINDIR} install); done

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
