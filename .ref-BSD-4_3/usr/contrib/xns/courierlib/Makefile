# $Header: Makefile,v 2.0 85/11/21 07:21:59 jqj Exp $

# $Log:	Makefile,v $
# Revision 2.0  85/11/21  07:21:59  jqj
# 4.3BSD standard release
# 
# Revision 1.1  85/11/04  11:27:24  jqj
# Initial revision
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

CFLAGS= -O
CRLIB = ${DESTDIR}/usr/new/lib/xnscourier
OUT = ${DESTDIR}/usr/local/lib/libcourier.a
OUT_P = ${DESTDIR}/usr/local/lib/libcourier_p.a
RCSREV=
RCSNAME=current
BINDIR=/usr/new

# order is important in the following:
SRCS=Time2.cr Authentication1.cr BulkData1.cr Clearinghouse2.cr \
		Authentication2.cr Printing3.cr

CRHDRS=Time2.h Authentication1.h BulkData1.h Clearinghouse2.h \
		Authentication2.h Printing3.h

# these names must be 12 chars or less for ar
OBJS=Time2.o Authent1.o BulkData1.o CH2.o Authent2.o Printing3.o CHEntries.o

.SUFFIXES: .h .cr

# build the standard library entries
install: all
	ar rv ${OUT} ${OBJS} *_c.o
	cd profiled; ar rv ${OUT_P} ${OBJS} *_c.o
	ranlib ${OUT} ${OUT_P}
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
	install -c -m 644 $*.h ${DESTDIR}/usr/include/xnscourier/$*.h
	install -c -m 644 $*.cr ${CRLIB}/$*.cr
.h.o:
	${CC} ${CFLAGS} -p -c $*_support.c
	mv $*_support.o profiled/$*.o
	${CC} ${CFLAGS} -c $*_support.c
	mv $*_support.o $*.o
	-if [ -f $*_client.c ]; then ${CC} ${CFLAGS} -p -c $*_client.c; \
	mv $*_client.o profiled/$*_c.o; \
	${CC} ${CFLAGS} -c $*_client.c; \
	mv $*_client.o $*_c.o;  fi

# CHEntries is a bit special
CHEntries.o: CHEntries0.o
	cp /usr/include/xnscourier/CHEntries0.h /usr/include/xnscourier/CHEntries.h
	cp CHEntries0.o CHEntries.o
	-mv profiled/CHEntries0.o profiled/CHEntries.o

# ar restricts the lengths of entry names
Authent1.o: Authentication1.o
	cp Authentication1.o Authent1.o
	-mv profiled/Authentication1.o profiled/Authent1.o

Authent2.o: Authentication2.o
	cp Authentication2.o Authent2.o
	-mv Authentication2_c.o Authent2_c.o
	-mv profiled/Authentication2.o profiled/Authent2.o
	-mv profiled/Authentication2_c.o profiled/Authent2_c.o

CH2.o: Clearinghouse2.o
	cp Clearinghouse2.o CH2.o
	-mv Clearinghouse2_c.o CH2_c.o
	-mv profiled/Clearinghouse2.o profiled/CH2.o
	-mv profiled/Clearinghouse2_c.o profiled/CH2_c.o

Time2.o: Time2.h

Authentication1.o: Authentication1.h

BulkData1.o: BulkData1.h

Clearinghouse2.o: Clearinghouse2.h

Authentication2.o: Authentication2.h

Printing3.o: Printing3.h

CHEntries0.o: CHEntries0.h

libcourier:
	@-if [ ! -d ${CRLIB} ]; then mkdir ${CRLIB}; fi;


Courierservices:
	rm -f Courierservices
	sed -e 's@/usr/local/lib/courier@${CRLIB}@g' services.txt >Courierservices
	cp Courierservices ${CRLIB}/Courierservices
	chmod 644 ${CRLIB}/Courierservices

clean:
	rm -f Courierservices *.h *.c *.o all profiled/*.[och] \
		profiled/all profile/install

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -q -u${RCSREV} -N${RCSNAME} RCS/*
