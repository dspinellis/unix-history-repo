CFLAGS=	-g -DDEBUG
#CFLAGS= -O
RCSREV=
RCSNAME=current
BINDIR=${DESTDIR}/usr/new
TESTS=enumerate listaddr listaliasof listproperties registerhost
CRLIB=${DESTDIR}/usr/new/lib/xnscourier

all: ${TESTS}

clean:
	-rm -f ${TESTS} *.o *.BAK *.CKP Cl*.[ch]

rcs:

install: all
	install -s -c registerhost ${BINDIR}

# test programs

enumerate.c listaddr.c listaliasof.c listproperties.c: Clearinghouse2_defs.h

Clearinghouse2_defs.h: ${CRLIB}/Clearinghouse2.cr
	${BINDIR}/xnscourier ${CRLIB}/Clearinghouse2.cr
	
enumerate: enumerate.o
	cc -g -o enumerate enumerate.o -lcourier

listaddr: listaddr.o
	cc -g -o listaddr listaddr.o -lcourier

listaliasof: listaliasof.o
	cc -g -o listaliasof listaliasof.o  -lcourier

listproperties: listproperties.o
	cc -g -o listproperties listproperties.o  -lcourier

registerhost: registerhost.o
	cc -g -o registerhost registerhost.o -lcourier

