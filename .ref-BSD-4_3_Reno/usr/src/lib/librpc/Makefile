#	@(#)Makefile	5.7 (Berkeley) 7/1/90

LIB=	rpc
CFLAGS+=-I${.CURDIR}
SRCS=	auth_none.c auth_unix.c authunix_prot.c clnt_perror.c clnt_raw.c \
	clnt_simple.c clnt_tcp.c clnt_udp.c pmap_clnt.c pmap_getmaps.c \
	pmap_getport.c pmap_prot.c pmap_rmt.c rpc_prot.c svc.c svc_auth.c \
	svc_auth_unix.c svc_raw.c svc_simple.c svc_tcp.c svc_udp.c xdr.c \
	xdr_array.c xdr_float.c xdr_mem.c xdr_rec.c xdr_reference.c \
	xdr_stdio.c
HDRS=	auth.h auth_unix.h clnt.h pmap_clnt.h pmap_prot.h rpc.h rpc_msg.h \
	svc.h svc_auth.h types.h xdr.h
MAN8=	portmap.0
.PATH: ${.CURDIR} ${.CURDIR}/doc
CLEANFILES=portmap

all: portmap

portmap: portmap.c librpc.a
	ranlib librpc.a
	${CC} ${CFLAGS} ${.CURDIR}/portmap.c librpc.a -lutil -o ${.TARGET}

beforeinstall: hdrs
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} portmap \
	    ${DESTDIR}/usr/sbin/portmap

hdrs: ${HDRS}
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${HDRS} \
	    ${DESTDIR}/usr/include/rpc

.include <bsd.lib.mk>
