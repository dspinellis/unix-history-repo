CFLAGS=-O
BINDIR = ${DESTDIR}/usr/new

all: getprintstatus printtest xnsprint

install: all
	install -s -c getprintstatus ${BINDIR}
	install -s -c xnsprint ${BINDIR}

xnsprint: xnsprint.o
	cc -g -o xnsprint xnsprint.o -lcourier

printtest: printtest.o
	cc -g -o printtest printtest.o  -lcourier

getprintstatus: getprintstatus.o
	cc -g -o getprintstatus getprintstatus.o -lcourier

Printing3_defs.h Printing3_support.c Printing3_client.c Printing3_server.c:
	/usr/new/xnscourier ${CRLIB}/Printing3.cr

xnsprint.o getprintstatus.o Printing3_client.o Printing3_server.o: Printing3_defs.h

clean:
	-rm -f *.o *.BAK *.CKP xnsprint printtest getprintstatus \
		Printing3_defs.h Printing3.h \
		Printing3_server.c Printing3_client.c Printing3_support.c

rcs:

