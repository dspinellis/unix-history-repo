CFLAGS=-g
LFLAGS=-g

passtest: passtest.o BasicAuthentication2_client.o getXNSpass.o creds.o
	${CC} ${LFLAGS} -o passtest passtest.o creds.o getXNSpass.o \
		BasicAuthentication2_client.o ${LIBS} -lcourier

BasicAuthentication2_defs.h BasicAuthentication2_support.c BasicAuthentication_2client.c BasicAuthentication2_server.c: BasicAuthentication2.cr
	/usr/new/xnscourier BasicAuthentication2.cr

passtest.o BasicAuthentication2_client.o BasicAuthentication2_server.o: BasicAuthentication2_defs.h

BasicAuthentication2_client.o BasicAuthentication2_server.o creds.c: BasicAuthentication2_support.c

clean:
	-rm -f *.o *.CKP *.BAK passtest BasicAuthentication2_client.c \
		BasicAuthentication2_server.c BasicAuthentication2_support.c \
		BasicAuthentication2_defs.h BasicAuthentication2.h

rcs:
install:
