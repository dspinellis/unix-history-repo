#	@(#)Makefile	5.11 (Berkeley) 9/6/90

.PATH: ${.CURDIR}/rpc ${.CURDIR}/.
LIB=	rpc
CFLAGS+=-I${.CURDIR} -I${.CURDIR}/rpc
SRCS+=	auth_none.c auth_unix.c  authunix_prot.c bindresvport.c \
	clnt_generic.c clnt_perror.c clnt_raw.c clnt_simple.c clnt_tcp.c \
	clnt_udp.c rpc_dtablesize.c get_myaddress.c getrpcent.c getrpcport.c \
	pmap_clnt.c pmap_getmaps.c pmap_getport.c pmap_prot.c \
	pmap_prot2.c pmap_rmt.c rpc_prot.c rpc_commondata.c rpc_callmsg.c \
	svc.c svc_auth.c svc_auth_unix.c svc_raw.c svc_run.c svc_simple.c \
	svc_tcp.c svc_udp.c xdr.c xdr_array.c xdr_mem.c \
	xdr_rec.c xdr_reference.c xdr_stdio.c

#
# XXX -- rstat.1 and rstat_svc.8 shouldn't really be here
#        but there's no rstat command, don't know why, so I'm
#        leaving them here in case they're needed sometime later.
# Paul.
#

MAN1+= rstat.1  
MAN3+= bindresvport.3 getrpcent.3 getrpcport.3 rpc.3 xdr.3
MAN5+= rpc.5
MAN8+= rstat_svc.8


UNSUPPORTED+=	xdr_float.c 

HDRS+=	auth.h auth_unix.h clnt.h pmap_clnt.h \
	pmap_prot.h pmap_rmt.h rpc.h rpc_msg.h svc.h svc_auth.h types.h xdr.h

all: librpc.a

beforeinstall: ${HDRS}
	@-if [ ! -d ${DESTDIR}/usr/include/rpc ]; then \
		mkdir ${DESTDIR}/usr/include/rpc; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/rpc; \
		chmod 755 ${DESTDIR}/usr/include/rpc; \
	fi
	cd ${.CURDIR}/rpc; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${HDRS} \
	    ${DESTDIR}/usr/include/rpc
