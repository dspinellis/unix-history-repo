CFLAGS=-O

sed:	sed0.o sed1.o
	cc -o sed *.o

sed0.o: sed0.c sed.h
sed1.o: sed1.c sed.h

install: sed
	install -s sed $(DESTDIR)/usr/bin
clean:
	rm -f *.o
