# Copyright 1985, Massachusetts Institute of Technology
DESTDIR=
INCLUDES= -I../include

CFLAGS= -O ${INCLUDES}
DBMLIB= -ldbm

OBJS= rgb.o

all: rgb

rgb: rgb.o
	${CC} -o rgb $(OBJS) $(DBMLIB)

install: all
	rgb ${DESTDIR}/usr/lib/rgb <rgb.txt
	cp rgb.txt ${DESTDIR}/usr/lib
	chmod 0644 ${DESTDIR}/usr/lib/rgb.txt

clean:
	rm -f *.o rgb a.out errs core gmon.out *~ *.bak *.BAK rgb.pag rgb.dir
