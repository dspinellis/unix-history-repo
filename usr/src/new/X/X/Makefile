# Copyright 1985, Massachusetts Institute of Technology.
#
# -DTCPCONN allows TCP connections
# you only need -DDUALTCP if you have both big and little Indian machines
# -DUNIXCONN allows Unix domain connections
# -DDNETCONN allows DECnet connections
INCLUDES= -I../include
NETOPTIONS= -DTCPCONN -DDUALTCP -DUNIXCONN
CFLAGS= -O ${NETOPTIONS} ${INCLUDES}
STDLIB= -ldbm

APOLLOLIB= ../libapollo/libapollo.a
VS100LIB= ../libvs100/libvs100.a
QVSSLIB= ../libqvss/libqvss.a
QILLIB= ../libqdss/libQIL.a
QDSSLIB= ../libqdss/libqdss.a ${QILLIB}
XSUNLIB= ../libsun/libsun.a
SUNLIB= $(XSUNLIB) -lsunwindow -lpixrect
NESTLIB= ../libnest/libnest.a ../Xlib/libX.a
XISLIB= ../libis/libis.a
ISLIB= $(XISLIB) -lgip
BITBLT= ../libibm/bitblt/bitblt.a
APA16LIB= ../libibm/libapa16/libapa16.a
APA8LIB= ../libibm/libapa8/libapa8.a
APA8CLIB= ../libibm/libapa8c/libapa8c.a
AEDLIB= ../libibm/libaed/libaed.a
PQDLIB= ../libibm/libpqd/libpqd.a

# you may want the CONFDIR set to somwhere on the root so your display
# can run at boot.  Fonts would also have to be stored there.
CONFDIR = /usr/new


OBJS= dispatch.o input.o main.o obscure.o overlap.o resource.o screen.o \
	window.o

all:

dec: vs100 qvss
#dec: vs100 qvss qdss

ibm: apa16 apa8 apa8c aed pqd

apollo: $(OBJS) lk201.o $(APOLLOLIB)
	${CC} -o servers/Xapollo $(OBJS) lk201.o $(STDLIB) $(APOLLOLIB)

vs100: $(OBJS) lk201.o $(VS100LIB)
	${CC} -o servers/Xvs100 $(OBJS) lk201.o $(STDLIB) $(VS100LIB)

qvss: $(OBJS) lk201.o $(QVSSLIB)
	${CC} -o servers/Xqvss $(OBJS) lk201.o $(STDLIB) $(QVSSLIB) 

qdss: $(OBJS) lk201.o $(QDSSLIB)
	${CC} -o servers/Xqdss $(OBJS) lk201.o $(STDLIB) $(QDSSLIB) 

sun: $(OBJS) $(XSUNLIB)
	${CC} ${CFLAGS} -o servers/Xsun $(OBJS) $(STDLIB) $(SUNLIB)

nest: $(OBJS) $(NESTLIB)
	${CC} -o servers/Xnest $(OBJS) $(STDLIB) $(NESTLIB) 

is: $(OBJS) $(XISLIB)
	${CC} ${CFLAGS} -o servers/Xis $(OBJS) $(STDLIB) $(ISLIB)

apa16: $(OBJS) $(APA16LIB) $(BITBLT)
	$(CC) -o servers/Xapa16 $(OBJS) $(STDLIB) $(APA16LIB) $(BITBLT)

apa8: $(OBJS) $(APA8LIB) $(BITBLT)
	$(CC) -o servers/Xapa8 $(OBJS) $(STDLIB) $(APA8LIB) $(BITBLT)

apa8c: $(OBJS) $(APA8CLIB) $(BITBLT)
	$(CC) -o servers/Xapa8c $(OBJS) $(STDLIB) $(APA8CLIB) $(BITBLT)

aed: $(OBJS) $(AEDLIB) $(BITBLT)
	$(CC) -o servers/Xaed $(OBJS) $(STDLIB) $(AEDLIB) $(BITBLT)

pqd: $(OBJS) $(PQDLIB) $(BITBLT)
	$(CC) -o servers/Xpqd $(OBJS) $(STDLIB) $(PQDLIB) $(BITBLT)

install: 
#	echo "Installing servers...."
#	cd servers; \
#	for i in X*; do \
#		install -c -m 775 $$i ${DESTDIR}${CONFDIR};\
#	done
	
include:
	install -c -m 0644 X.h      ${DESTDIR}/usr/include/X
	install -c -m 0644 rgb.h    ${DESTDIR}/usr/include/X
	install -c -m 0644 Xproto.h ${DESTDIR}/usr/include/X
	install -c -m 0644 mit-copyright.h ${DESTDIR}/usr/include/X

lint:
	lint ${NETOPTIONS} -I${INCDIR} *.c ../libvs100/llib-lvs100.ln

dispatch.o: Xint.h Xdev.h X.h Xproto.h
input.o: Xint.h Xdev.h X.h Xproto.h
main.o: Xint.h Xdev.h X.h Xproto.h
obscure.o: Xint.h Xdev.h X.h Xproto.h
overlap.o: Xint.h Xdev.h X.h Xproto.h
resource.o: Xint.h Xdev.h X.h Xproto.h
screen.o: Xint.h Xdev.h X.h Xproto.h
window.o: Xint.h Xdev.h X.h Xproto.h

clean:
	rm -f \#* *.o a.out errs core gmon.out *~
	rm -f servers/X*
