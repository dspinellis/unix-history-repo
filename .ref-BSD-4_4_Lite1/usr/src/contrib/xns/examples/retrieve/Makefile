CFLAGS=-g

retrieve: retrieve.o
	${CC} -g -o retrieve retrieve.o  -lcourier

retrieve.o: Clearinghouse2_defs.h

Clearinghouse2_defs.h: Clearinghouse2.h
	cp ../../courierlib/Clearinghouse2_defs.h .

Clearinghouse2.h:
	cp ../../courierlib/Clearinghouse2.h .

clean:
	-rm -f *.o *.BAK *.CKP retrieve Clearinghouse2_defs.h \
		Clearinghouse2_support.c Clearinghouse2.h\
		Clearinghouse2_client.c Clearinghouse2_server.c

rcs:

install: retrieve

