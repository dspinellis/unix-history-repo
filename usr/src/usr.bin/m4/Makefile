Virgin BTL M4 as sent out in 4.1
# @(#)Makefile	5.2 (Berkeley) %G%

CFLAGS=-O
m4:	m4.o m4y.o
	$(CC) -o m4 m4.o m4y.o 

install:
	install -s m4 $(DESTDIR)/usr/bin

clean:
	rm -f *.o

depend:
