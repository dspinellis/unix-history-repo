# $Header: Makefile,v 1.3 87/04/01 10:14:19 ed Exp $
#
# Copyright (c) 1986, 1987 Xerox Corporation.
#
# $Log:	Makefile,v $
# Revision 1.3  87/04/01  10:14:19  ed
# Added ROOTNOTALLOWED compilation flag for services.
# 
# Revision 1.2  87/03/17  16:31:21  ed
# Everything now made here; avoids replication into other directories.
# 
# Revision 1.1  87/01/16  09:39:03  ed
# Initial revision
# 
# Revision 1.1  87/01/14  13:05:16  ed
# Initial revision
# 
# 

DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

BINDIR = ../../bin
LIBDIR = ../../lib
MANDIR = ../../man
INCDIR = ../../include
COURIERDIR = ../../xnscourier

#
# compile flags
#   FILING4=1			defines use of Filing Protocol v4
#   FILING5=1			defines use of Filing Protocol v5
#   FILING6=1			defines use of Filing Protocol v6
#   FILINGSUBSET1=1		defines use of FilingSubset Protocol v1
#   FILETOOLCOMPATIIBLITY	allows use with XDE Filetool until FilingSubset
#				is implemented
#   EXTENSIONS			enable services beyond FilingSubset compatiblity
#   ROONOTALLOWED		disable "root" logins
#   DEBUG			debug output generated
# 

#CFLAGS= -O -g -DDEBUG -I${INCDIR}
CFLAGS= -O -I. -I${INCDIR}
RCSREV=

# directories
INCLUDE= ${INCDIR}/xnscourier

OBJS= misc.o get_type.o errmsg.o Make2ndCreds.o 
F4OBJS= filingd_F4.o system_F4.o errors_F4.o \
	attribute_F4.o VPsupport_F4.o 
F5OBJS= filingd_F5.o system_F5.o errors_F5.o \
	attribute_F5.o VPsupport_F5.o 
F6OBJS= filingd_F6.o system_F6.o errors_F6.o \
	attribute_F6.o VPsupport_F6.o unixcreds_F6.o
FS1OBJS= filingd_FS1.o system_FS1.o errors_FS1.o \
	attribute_FS1.o VPsupport_FS1.o unixcreds_FS1.o

HDRS= ${INCLUDE}/filetypes.h ${INCLUDE}/filing_server.h

ALL= ${OBJS} ${F4OBJS} ${F5OBJS} ${F6OBJS} ${FS1OBJS}

install: ${HDRS} ${ALL}
	ar cr ${LIBDIR}/lib_filing.a ${OBJS}
	ranlib ${LIBDIR}/lib_filing.a
	ar cr ${LIBDIR}/lib_filing4.a ${F4OBJS}
	ranlib ${LIBDIR}/lib_filing4.a
	ar cr ${LIBDIR}/lib_filing5.a ${F5OBJS}
	ranlib ${LIBDIR}/lib_filing5.a
	ar cr ${LIBDIR}/lib_filing6.a ${F6OBJS}
	ranlib ${LIBDIR}/lib_filing6.a
	ar cr ${LIBDIR}/lib_filingsubset1.a ${FS1OBJS}
	ranlib ${LIBDIR}/lib_filingsubset1.a

errmsg.o: errmsg.c
	${CC} -c -I../../courierlib -I${INCDIR} -I${INCLUDE} errmsg.c

get_type.o: get_type.c 
	${CC} -c ${CFLAGS} get_type.c

Make2ndCreds.o: MakeSecondaryCreds.c ${INCLUDE}/FilingSubset1.h
	${CC} -c ${CFLAGS} -o Make2ndCreds.o MakeSecondaryCreds.c

#
# filing V4
#

filingd_F4.o : filingd.c filingV4.h \
			clearinghouseV2.h authenticationV2.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILING4=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		-DROOTNOTALLOWED filingd.c -o filingd_F4.o

errors_F4.o : filing_errors.c filingV4.h \
			clearinghouseV2.h authenticationV2.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILING4=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		filing_errors.c -o errors_F4.o

system_F4.o : system_interface.c filingV4.h \
			clearinghouseV2.h authenticationV2.h
	${CC} -c ${CFLAGS} -DFILING4=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		system_interface.c -o system_F4.o

attribute_F4.o : attribute.c filingV4.h \
			clearinghouseV2.h authenticationV2.h
	${CC} -c ${CFLAGS} -DFILING4=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		attribute.c -o attribute_F4.o

VPsupport_F4.o : VPsupport.c filingV4.h \
			clearinghouseV2.h authenticationV2.h
	${CC} -c ${CFLAGS} -DFILING4=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		VPsupport.c -o VPsupport_F4.o

#
# filing V5
#

filingd_F5.o : filingd.c filingV5.h \
			clearinghouseV2.h authenticationV2.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILING5=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		-DROOTNOTALLOWED filingd.c -o filingd_F5.o

errors_F5.o : filing_errors.c filingV5.h \
			clearinghouseV2.h authenticationV2.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILING5=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		filing_errors.c -o errors_F5.o

system_F5.o : system_interface.c filingV5.h \
			clearinghouseV2.h authenticationV2.h
	${CC} -c ${CFLAGS} -DFILING5=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		system_interface.c -o system_F5.o

attribute_F5.o : attribute.c filingV5.h \
			clearinghouseV2.h authenticationV2.h
	${CC} -c ${CFLAGS} -DFILING5=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		attribute.c -o attribute_F5.o

VPsupport_F5.o : VPsupport.c filingV5.h \
			clearinghouseV2.h authenticationV2.h
	${CC} -c ${CFLAGS} -DFILING5=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		VPsupport.c -o VPsupport_F5.o

#
# filing V6
#

filingd_F6.o : filingd.c filingV6.h \
			clearinghouseV3.h authenticationV3.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILING6=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		-DROOTNOTALLOWED filingd.c -o filingd_F6.o

errors_F6.o : filing_errors.c filingV6.h \
			clearinghouseV3.h authenticationV3.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILING6=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		filing_errors.c -o errors_F6.o

system_F6.o : system_interface.c filingV6.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILING6=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		system_interface.c -o system_F6.o

attribute_F6.o : attribute.c filingV6.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILING6=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		attribute.c -o attribute_F6.o

VPsupport_F6.o : VPsupport.c filingV6.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILING6=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		VPsupport.c -o VPsupport_F6.o

unixcreds_F6.o : unixcreds.c filingV6.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILING6=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		unixcreds.c -o unixcreds_F6.o

#
# filingsubset V1
#

filingd_FS1.o : filingd.c filingsubsetV1.h \
			clearinghouseV3.h authenticationV3.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILINGSUBSET1=1 \
		-DROOTNOTALLOWED filingd.c -o filingd_FS1.o

errors_FS1.o : filing_errors.c filingsubsetV1.h \
			clearinghouseV3.h authenticationV3.h
	@echo 'expect enumeration type clashes (for now)'
	${CC} -c ${CFLAGS} -DFILINGSUBSET1=1 \
		filing_errors.c -o errors_FS1.o

system_FS1.o : system_interface.c filingsubsetV1.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILINGSUBSET1=1 \
		system_interface.c -o system_FS1.o

attribute_FS1.o : attribute.c filingsubsetV1.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILINGSUBSET1=1 \
		attribute.c -o attribute_FS1.o

VPsupport_FS1.o : VPsupport.c filingsubsetV1.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILINGSUBSET1=1 \
		VPsupport.c -o VPsupport_FS1.o

unixcreds_FS1.o : unixcreds.c filingsubsetV1.h \
			clearinghouseV3.h authenticationV3.h
	${CC} -c ${CFLAGS} -DFILINGSUBSET1=1 -DFILETOOLCOMPATIBILITY -DEXTENSIONS \
		unixcreds.c -o unixcreds_FS1.o

#
# headers
#

${INCLUDE}/filetypes.h: filetypes.h
	install -c -m 644 filetypes.h ${INCLUDE}/filetypes.h

${INCLUDE}/filing_server.h: filing_server.h
	install -c -m 644 filing_server.h ${INCLUDE}/filing_server.h

clean:
	-rm -f *.o *.BAK *.CKP ${ALL} \
		filingV4.h filingV5.h filingV6.h filingsubsetV1.h \
		clearinghouseV2.h clearinghouseV3.h \
		authenticationV2.h authenticationV3.h

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

filingV4.h: ../../courierlib/Filing4_defs.h 
	sed -e 's/typedef Filing4_[^ ]* /&FILING_/' \
	    -e 's/#define Filing4_[^ ]* /&FILING_/g' \
	    -e '/#define Filing4_/!s/#define /#define FILING_/g' \
	    -e 's/^	/&FILING_/' \
	    -e 's/^} /&FILING_/' \
	    -e 's/#include "Filing4.h"/#include <xnscourier\/Filing4.h>/' \
	    ../../courierlib/Filing4_defs.h >filingV4.h

filingV5.h: ../../courierlib/Filing5_defs.h
	sed -e 's/typedef Filing5_[^ ]* /&FILING_/' \
	    -e 's/#define Filing5_[^ ]* /&FILING_/g' \
	    -e '/#define Filing5_/!s/#define /#define FILING_/g' \
	    -e 's/^	/&FILING_/' \
	    -e 's/^} /&FILING_/' \
	    -e 's/#include "Filing5.h"/#include <xnscourier\/Filing5.h>/' \
	    ../../courierlib/Filing5_defs.h >filingV5.h

filingV6.h: ../../courierlib/Filing6_defs.h
	sed -e 's/typedef Filing6_[^ ]* /&FILING_/' \
	    -e 's/#define Filing6_[^ ]* /&FILING_/g' \
	    -e '/#define Filing6_/!s/#define /#define FILING_/g' \
	    -e 's/^	/&FILING_/' \
	    -e 's/^} /&FILING_/' \
	    -e 's/#include "Filing6.h"/#include <xnscourier\/Filing6.h>/' \
	    ../../courierlib/Filing6_defs.h >filingV6.h

filingsubsetV1.h: ../../courierlib/FilingSubset1_defs.h
	sed -e 's/typedef FilingSubset1_[^ ]* /&FILING_/' \
	    -e 's/#define FilingSubset1_[^ ]* /&FILING_/g' \
	    -e '/#define FilingSubset1_/!s/#define /#define FILING_/g' \
	    -e 's/^	/&FILING_/' \
	    -e 's/^} /&FILING_/' \
	    -e 's/#include "FilingSubset1.h"/#include <xnscourier\/FilingSubset1.h>/' \
	    ../../courierlib/FilingSubset1_defs.h >filingsubsetV1.h

clearinghouseV2.h: ../../courierlib/Clearinghouse2_defs.h
	sed -e 's/typedef Clearinghouse2_[^ ]* /&CLEARINGHOUSE_/' \
	    -e 's/#define Clearinghouse2_[^ ]* /&CLEARINGHOUSE_/g' \
	    -e '/#define Clearinghouse2_/!s/#define /#define CLEARINGHOUSE_/g' \
	    -e 's/^	/&CLEARINGHOUSE_/' \
	    -e 's/^} /&CLEARINGHOUSE_/' \
	    -e '/#include/D' \
	    ../../courierlib/Clearinghouse2_defs.h >clearinghouseV2.h

clearinghouseV3.h: ../../courierlib/Clearinghouse3_defs.h
	sed -e 's/typedef Clearinghouse3_[^ ]* /&CLEARINGHOUSE_/' \
	    -e 's/#define Clearinghouse3_[^ ]* /&CLEARINGHOUSE_/g' \
	    -e '/#define Clearinghouse3_/!s/#define /#define CLEARINGHOUSE_/g' \
	    -e 's/^	/&CLEARINGHOUSE_/' \
	    -e 's/^} /&CLEARINGHOUSE_/' \
	    -e '/#include/D' \
	    ../../courierlib/Clearinghouse3_defs.h >clearinghouseV3.h

authenticationV2.h: ../../courierlib/Authentication2_defs.h
	sed -e 's/typedef Authentication2_[^ ]* /&AUTHENTICATION_/' \
	    -e 's/#define Authentication2_[^ ]* /&AUTHENTICATION_/g' \
	    -e '/#define Authentication2_/!s/#define /#define AUTHENTICATION_/g' \
	    -e 's/^	/&AUTHENTICATION_/' \
	    -e 's/^} /&AUTHENTICATION_/' \
	    -e '/#include/D' \
	    ../../courierlib/Authentication2_defs.h >authenticationV2.h

authenticationV3.h: ../../courierlib/Authentication3_defs.h
	sed -e 's/typedef Authentication3_[^ ]* /&AUTHENTICATION_/' \
	    -e 's/#define Authentication3_[^ ]* /&AUTHENTICATION_/g' \
	    -e '/#define Authentication3_/!s/#define /#define AUTHENTICATION_/g' \
	    -e 's/^	/&AUTHENTICATION_/' \
	    -e 's/^} /&AUTHENTICATION_/' \
	    -e '/#include/D' \
	    ../../courierlib/Authentication3_defs.h >authenticationV3.h


