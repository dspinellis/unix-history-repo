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

VS100LIB= ../libvs100/libvs100.a
QVSSLIB= ../libqvss/libqvss.a
QILLIB= ../libqdss/libQIL.a
QDSSLIB= ../libqdss/libqdss.a ${QILLIB}
XSUNLIB= ../libsun/libsun.a
SUNLIB= $(XSUNLIB) -lsunwindow -lpixrect
NESTLIB= ../libnest/libnest.a ../Xlib/libX.a

# you may want the CONFDIR set to somwhere on the root so your display
# can run at boot.  Fonts would also have to be stored there.
CONFDIR = /usr/new


OBJS= dispatch.o input.o main.o obscure.o overlap.o resource.o screen.o \
	window.o

all: $(OBJS) lk201.o vs100 nest

vs100: $(OBJS) lk201.o $(VS100LIB)
	${CC} -o Xvs100 $(OBJS) lk201.o $(STDLIB) $(VS100LIB)

qvss: $(OBJS) lk201.o $(QVSSLIB)
	${CC} -o Xqvss $(OBJS) lk201.o $(STDLIB) $(QVSSLIB) 

qdss: $(OBJS) lk201.o $(QDSSLIB)
	${CC} -o Xqdss $(OBJS) lk201.o $(STDLIB) $(QDSSLIB) 

sun: $(OBJS) $(XSUNLIB)
	${CC} ${CFLAGS} -o Xsun $(OBJS) $(STDLIB) $(SUNLIB)

nest: $(OBJS) $(NESTLIB)
	${CC} -o Xnest $(OBJS) $(STDLIB) $(NESTLIB) 

install: all
	install Xvs100 ${DESTDIR}${CONFDIR}
#	install Xqvss  ${DESTDIR}${CONFDIR}
#	install Xqdss  ${DESTDIR}${CONFDIR}
#	install Xsun   ${DESTDIR}${CONFDIR}

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
	rm -f \#* *.o Xvs100 Xqvss Xqdss Xsun Xnest a.out errs core gmon.out *~
