# $Header: Makefile,v 2.5 87/05/11 09:45:54 ed Exp $

# $Log:	Makefile,v $
# Revision 2.5  87/05/11  09:45:54  ed
# Compile server routines.
# Added Filing (4,5,6) and FilingSubset (1).
# 
# Revision 2.4  87/02/14  13:49:00  jqj
# new directory structure
# 
# Revision 2.4  87/01/16  15:23:37  ed
# Get include files from right place
# 
# Revision 2.3  87/01/16  09:58:33  ed
# Added Authentication and Clearinghouse versions 3
# Updated to new directory structure
# 
# Revision 2.2  86/05/12  09:33:14  jqj
# Authentication1 might have procedures.
# 
# Revision 2.1  85/12/17  07:53:01  jqj
# cleaned up some comments
# 
# Revision 2.0  85/11/21  07:21:59  jqj
# 4.3BSD standard release
# 
# Revision 1.5  85/05/23  06:21:01  jqj
# Public Beta-test version, released 24 May 1985
# 
# Revision 1.4  85/03/26  06:11:32  jqj
# Revised public alpha-test version, released 26 March 1985
# 
# Revision 1.3  85/03/12  04:49:31  jqj
# added "clean" entry point
# 
# Revision 1.2  85/03/11  16:44:09  jqj
# Public alpha-test version, released 11 March 1985
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

BINDIR = ../bin
LIBDIR = ../lib
MANDIR = ../man
INCDIR = ../include
COURIERDIR = ../xnscourier

CFLAGS= -O -I${INCDIR}
OUT = ${LIBDIR}/libcourier.a
OUT_P = ${LIBDIR}/libcourier_p.a
RCSREV=
RCSNAME=current

# order is important in the following:
SRCS=Time2.cr Authentication1.cr BulkData1.cr Clearinghouse2.cr \
		Authentication2.cr Printing3.cr \
		Authentication3.cr Clearinghouse3.cr \
		Filing4.cr Filing5.cr Filing6.cr FilingSubset1.cr

CRHDRS=Time2.h Authentication1.h BulkData1.h Clearinghouse2.h \
		Authentication2.h Printing3.h \
		Authentication3.h Clearinghouse3.h \
		Filing4.h Filing5.h Filing6.h FilingSubset1.h

# these names must be 12 chars or less for ar
OBJS=Time2.o Authent1.o BulkData1.o CH2.o Authent2.o Printing3.o CHEntries.o \
		Authent3.o CH3.o Filing4.o Filing5.o Filing6.o FSubset1.o

.SUFFIXES: .h .cr

# build the standard library entries
install: all
	ar rv ${OUT} ${OBJS} *_c.o
	cd profiled; ar rv ../${OUT_P} ${OBJS} *_c.o
	ranlib ${OUT} ${OUT_P}
	for i in ${SRCS}; do \
		(install -c -m 644 $$i ${COURIERDIR}/$$i); done
	for i in ${CRHDRS}; do \
		(install -c -m 644 $$i ${INCDIR}/xnscourier/$$i); done
	install -c -m 644 CHEntries0.cr ${COURIERDIR}/CHEntries0.cr
	cp CHEntries0.h ${INCDIR}/xnscourier/CHEntries.h
	cp CHEntries0.h ${INCDIR}/xnscourier/CHEntries0.h
	cp Courierservices ${COURIERDIR}/Courierservices
	chmod 644 ${COURIERDIR}/Courierservices
	date > install

# order is important here too
all:	libcourier Courierservices ${CRHDRS} ${OBJS}


# for the following to work, we first need:
#  1) /usr/local/lib/courier and /usr/include/courier directories
#  2) Courierservices in its appropriate place
#  3) for any DEPENDS UPON construct, the associated .h and .cr
#     files must already be installed

.cr.h:
	${BINDIR}/xnscourier $*.cr
	install -c -m 644 $*.h ${INCDIR}/xnscourier/$*.h
	install -c -m 644 $*.cr ${COURIERDIR}/$*.cr
.h.o:
	${CC} ${CFLAGS} -p -c $*_support.c
	mv $*_support.o profiled/$*.o
	${CC} ${CFLAGS} -c $*_support.c
	mv $*_support.o $*.o
	-if [ -f $*_client.c ]; then ${CC} ${CFLAGS} -p -c $*_client.c; \
	mv $*_client.o profiled/$*_c.o; \
	${CC} ${CFLAGS} -c $*_client.c; \
	mv $*_client.o $*_c.o;  fi
	-if [ -f $*_server.c ]; then ${CC} ${CFLAGS} -p -c $*_server.c; \
	mv $*_server.o profiled/$*_s.o; \
	${CC} ${CFLAGS} -c $*_server.c; \
	mv $*_server.o $*_s.o;  fi


# CHEntries is a bit special
CHEntries.o: CHEntries0.o
	cp ${INCDIR}/xnscourier/CHEntries0.h ${INCDIR}/xnscourier/CHEntries.h
	cp CHEntries0.o CHEntries.o
	-mv profiled/CHEntries0.o profiled/CHEntries.o

# ar restricts the lengths of entry names
Authent1.o: Authentication1.o
	cp Authentication1.o Authent1.o
	-mv Authentication1_c.o Authent1_c.o
	-mv Authentication1_s.o Authent1_s.o
	-mv profiled/Authentication1.o profiled/Authent1.o
	-mv profiled/Authentication1_c.o profiled/Authent1_c.o
	-mv profiled/Authentication1_s.o profiled/Authent1_s.o

Authent2.o: Authentication2.o
	cp Authentication2.o Authent2.o
	-mv Authentication2_c.o Authent2_c.o
	-mv Authentication2_s.o Authent2_s.o
	-mv profiled/Authentication2.o profiled/Authent2.o
	-mv profiled/Authentication2_c.o profiled/Authent2_c.o
	-mv profiled/Authentication2_s.o profiled/Authent2_s.o

Authent3.o: Authentication3.o
	cp Authentication3.o Authent3.o
	-mv Authentication3_c.o Authent3_c.o
	-mv Authentication3_s.o Authent3_s.o
	-mv profiled/Authentication3.o profiled/Authent3.o
	-mv profiled/Authentication3_c.o profiled/Authent3_c.o
	-mv profiled/Authentication3_s.o profiled/Authent3_s.o

CH2.o: Clearinghouse2.o
	cp Clearinghouse2.o CH2.o
	-mv Clearinghouse2_c.o CH2_c.o
	-mv Clearinghouse2_s.o CH2_s.o
	-mv profiled/Clearinghouse2.o profiled/CH2.o
	-mv profiled/Clearinghouse2_c.o profiled/CH2_c.o
	-mv profiled/Clearinghouse2_s.o profiled/CH2_s.o

CH3.o: Clearinghouse3.o
	cp Clearinghouse3.o CH3.o
	-mv Clearinghouse3_c.o CH3_c.o
	-mv Clearinghouse3_s.o CH3_s.o
	-mv profiled/Clearinghouse3.o profiled/CH3.o
	-mv profiled/Clearinghouse3_c.o profiled/CH3_c.o
	-mv profiled/Clearinghouse3_s.o profiled/CH3_s.o

FSubset1.o: FilingSubset1.o
	cp FilingSubset1.o FSubset1.o
	-mv FilingSubset1_c.o FSubset1_c.o
	-mv FilingSubset1_s.o FSubset1_s.o
	-mv profiled/FilingSubset1.o profiled/FSubset1.o
	-mv profiled/FilingSubset1_c.o profiled/FSubset1_c.o
	-mv profiled/FilingSubset1_s.o profiled/FSubset1_s.o

Time2.o: Time2.h

Authentication1.o: Authentication1.h

BulkData1.o: BulkData1.h

Clearinghouse2.o: Clearinghouse2.h

Authentication2.o: Authentication2.h

Printing3.o: Printing3.h

CHEntries0.o: CHEntries0.h

Authentication3.o: Authentication3.h

Clearinghouse3.o: Clearinghouse3.h

Filing4.o: Filing4.h

Filing5.o: Filing5.h

Filing6.o: Filing6.h

FilingSubset1.o: FilingSubset1.h

libcourier:
	@-if [ ! -d ${COURIERDIR} ]; then mkdir ${COURIERDIR}; fi;


Courierservices:
	rm -f Courierservices
	sed -e 's@/usr/local/lib/courier@${COURIERDIR}@g' services.txt >Courierservices

clean:
	rm -f *.BAK *.CKP Courierservices *.h *.c *.o all profiled/*.[och] \
		profiled/all profile/install

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -q -u${RCSREV} -N${RCSNAME} RCS/*

