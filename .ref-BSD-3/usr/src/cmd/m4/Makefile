CFLAGS=-O
m4:	m4.o m4y.o
	$(CC) -o m4 m4.o m4y.o 

install:
	install -s m4 $(DESTDIR)/usr/bin

clean:
	rm -f *.o
