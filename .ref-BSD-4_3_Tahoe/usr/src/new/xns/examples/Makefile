# $Header: Makefile,v 1.6 87/05/11 09:52:26 ed Exp $
#
# $Log:	Makefile,v $
# Revision 1.6  87/05/11  09:52:26  ed
# Added improved Filing client, Filing servers and other clients.
# 
# Revision 1.5  87/02/14  13:49:44  jqj
# new directory structure
# 
# Revision 1.4  87/01/05  11:44:10  ed
# Updated to new directory structure
# 
# Revision 1.3  86/06/25  13:03:31  jqj
# added $CC for subsidiary makefiles
# 
# Revision 1.2  85/12/17  07:54:12  jqj
# none
# 
#

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTLIB = ${DESTDIR}/usr/new/lib
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier
DESTETC = ${DESTDIR}/etc

BINDIR = ../bin
ETCDIR = ../etc
LIBDIR = ../lib
MANDIR = ../man
INCDIR = ../include
COURIERDIR = ../xnscourier

MFLAGS = DESTDIR=${DESTDIR} DESTBIN=${DESTBIN} DESTCOURIER=${DESTCOURIER} \
	DESTETC=${DESTETC} BINDIR=../${BINDIR} LIBDIR=../${LIBDIR} \
	ETCDIR=../${ETCDIR} MANDIR=../${MANDIR} INCDIR=../${INCDIR} \
	COURIERDIR=../${COURIERDIR}

SUBDIRS=print gap ch xnscreds \
	filing_common filing_servers filing_client \
	remotetool archive vpfile
BROKEN=authchtest retrieve auth 

all:
	-for Dir in ${SUBDIRS}; do \
		(cd $$Dir; make ${MFLAGS} CC=${CC}); done

install:
	-for Dir in ${SUBDIRS}; do \
		(cd $$Dir; \
		make ${MFLAGS} CC=${CC} install); \
		done

rcs:
	-for Dir in ${SUBDIRS}; do (cd $$Dir; make rcs); done

clean:
	-rm -f *.BAK *.CKP
	-for Dir in ${SUBDIRS}; do (cd $$Dir; make clean); done

