CFLAGS=-g

all: chtest chlookup

install: all

chtest: chtest.o getdata.o Clearinghouse_client.o \
	creds.o hashpass.o
	${CC} -g -o chtest chtest.o getdata.o Clearinghouse_client.o \
			creds.o getXNSpass.o hashpass.o -lcourier

chlookup: chlookup.o Clearinghouse_client.o creds.o getXNSpass.o hashpass.o
	${CC} -g -o chlookup chlookup.o Clearinghouse_client.o \
			creds.o getXNSpass.o hashpass.o -lcourier

Clearinghouse_defs.h Clearinghouse_support.c Clearinghouse_client.c Clearinghouse_server.c: Clearinghouse.cr
	/usr/new/xnscourier Clearinghouse.cr

chlookup.o getdata.o chtest.o Clearinghouse_client.o Clearinghouse_server.o: Clearinghouse_defs.h

getdata.o Clearinghouse_client.o Clearinghouse_server.o: Clearinghouse_support.c

clean:
	-rm -f *.CKP *.BAK chtest chlookup \
		Clearinghouse_defs.h Clearinghouse_support.c \
		Clearinghouse_client.c Clearinghouse_server.c *.o

rcs:
