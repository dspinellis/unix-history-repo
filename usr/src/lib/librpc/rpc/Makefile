#
# 	Makefile	1.3	85/02/08
#
DESTDIR=

SRC=	auth_none.c auth_unix.c authunix_prot.c \
	clnt_perror.c clnt_raw.c clnt_simple.c clnt_tcp.c clnt_udp.c \
	pmap_clnt.c pmap_getmaps.c pmap_getport.c pmap_prot.c pmap_rmt.c \
	rpc_prot.c \
	svc.c svc_auth.c svc_auth_unix.c svc_raw.c svc_simple.c \
	svc_tcp.c svc_udp.c xdr.c xdr_array.c xdr_float.c xdr_mem.c xdr_rec.c \
	xdr_reference.c xdr_stdio.c

OBJ=	auth_none.o auth_unix.o authunix_prot.o clnt_perror.o clnt_raw.o\
	clnt_simple.o clnt_tcp.o clnt_udp.o \
	pmap_clnt.o pmap_getmaps.o pmap_getport.o pmap_prot.o pmap_rmt.o \
	rpc_prot.o \
	svc.o svc_auth.o svc_auth_unix.o svc_raw.o svc_simple.o \
	svc_tcp.o svc_udp.o xdr.o xdr_array.o xdr_float.o xdr_mem.o xdr_rec.o \
	xdr_reference.o xdr_stdio.o
INC=	auth.h auth_unix.h clnt.h pmap_clnt.h\
	pmap_prot.h rpc.h rpc_msg.h svc.h svc_auth.h types.h xdr.h

CFLAGS= -O -I.

.c.o:
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

all: rpclib portmap rpcinfo

portmap: portmap.o
	cc $(CFLAGS) portmap.o rpclib -o portmap

rpcinfo: rpcinfo.o
	cc $(CFLAGS) rpcinfo.o rpclib -o rpcinfo

rpclib rpclib_p: ${OBJ}
	@echo "building profiled rpclib"
	@cd profiled; ar cru ../rpclib_p ${OBJ}
	@echo "building normal rpclib"
	@ar cru rpclib ${OBJ}
	ranlib rpclib

install: all
	-mkdir ${DESTDIR}/usr/include/rpc && \
		chmod 755 ${DESTDIR}/usr/include/rpc
	-for i in *.h; do \
		(install -c -m 644 $$i ${DESTDIR}/usr/include/rpc) done
	install rpclib ${DESTDIR}/usr/new/lib/librpc.a
	ranlib ${DESTDIR}/usr/new/lib/librpc.a
	install -s portmap ${DESTDIR}/usr/new/portmap
	install -s rpcinfo ${DESTDIR}/usr/new/rpcinfo
	install -c -m 444 doc/rpcinfo.8 ${DESTDIR}/usr/man/mann/rpcinfo.n
	install -c -m 444 doc/portmap.8c ${DESTDIR}/usr/man/mann/portmap.n

tags: $(SRC) $(KSRC) $(INC)
	ctags -tw $(SRC) $(KSRC) $(INC)

ref: tags
	sed 's,	/.*,,' tags | \
	awk ' { printf("%-26s%-16s%s\n", $$1, $$2, $$3) }' > ref

lint:
	lint -bnuvx $(SRC)

print:
	pr $(INC) $(SRC) $(KSRC) | lpr -Pvp

clean:
	rm -f $(OBJ) librpc.a linted made profiled/*.o portmap.o rpcinfo.o

depend:
	@-grep '^#include' $(SRC) | grep -v '<' | grep -v '../' | \
	sed 's/:[^"]*"\([^"]*\)".*/: \1/' | sed 's/\.[cs]:/.o:/' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' >> makedep
	@echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	@echo '$$r makedep' >>eddep
	@echo 'w' >>eddep
	@cp Makefile makefile.bak
	@ed - Makefile < eddep
	@rm eddep makedep makefile.bak



# DO NOT DELETE THIS LINE

auth_none.o: types.h xdr.h auth.h
auth_unix.o: types.h xdr.h auth.h auth_unix.h
authunix_prot.o: types.h xdr.h auth.h auth_unix.h
clnt_perror.o: types.h xdr.h auth.h clnt.h rpc_msg.h
clnt_raw.o: types.h xdr.h auth.h clnt.h rpc_msg.h
clnt_tcp.o: types.h xdr.h auth.h clnt.h rpc_msg.h pmap_clnt.h
clnt_udp.o: types.h xdr.h auth.h clnt.h rpc_msg.h pmap_clnt.h
pmap_clnt.o: types.h xdr.h auth.h clnt.h rpc_msg.h pmap_prot.h pmap_clnt.h
pmap_getmaps.o: types.h xdr.h auth.h clnt.h rpc_msg.h pmap_prot.h pmap_clnt.h
pmap_getport.o: types.h xdr.h auth.h clnt.h rpc_msg.h pmap_prot.h pmap_clnt.h
pmap_prot.o: types.h xdr.h pmap_prot.h
pmap_rmt.o: types.h xdr.h auth.h clnt.h rpc_msg.h pmap_prot.h pmap_clnt.h
rpc_prot.o: types.h xdr.h auth.h clnt.h rpc_msg.h
svc.o: types.h xdr.h auth.h clnt.h rpc_msg.h svc.h svc_auth.h
svc_auth.o: types.h xdr.h auth.h clnt.h rpc_msg.h svc.h svc_auth.h
svc_auth_unix.o: types.h xdr.h auth.h clnt.h rpc_msg.h svc.h auth_unix.h
svc_auth_unix.o: svc_auth.h
svc_raw.o: types.h xdr.h auth.h clnt.h rpc_msg.h svc.h
svc_tcp.o: types.h xdr.h auth.h clnt.h rpc_msg.h svc.h
svc_udp.o: types.h xdr.h auth.h clnt.h rpc_msg.h svc.h
xdr.o: types.h xdr.h
xdr_array.o: types.h xdr.h
xdr_float.o: types.h xdr.h
xdr_mem.o: types.h xdr.h
xdr_rec.o: types.h xdr.h
xdr_reference.o: types.h xdr.h
xdr_stdio.o: types.h xdr.h
