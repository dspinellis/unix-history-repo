CFLAGS = -O

SOURCES = x25err.c getx25hostent.c getx25hostbyname.c getx25hostbyaddr.c ccitt_addr.c print.c verify.c

OBJECTS = x25err.o getx25hostent.o getx25hostbyname.o getx25hostbyaddr.o ccitt_addr.o print.o verify.o

HEADERS = /sys/netccitt/x25.h ../h/x29.h

x25lib.a: $(OBJECTS)
	ar rv x25lib.a $?
	ranlib x25lib.a

install: x25lib.a
	install x25lib.a /usr/lib/libx25.a
	ranlib /usr/lib/libx25.a

.c.o:
	cc -c $(CFLAGS) $*.c


$(OBJECTS): $(HEADERS)

clean:
	rm -f core *.o *.CKP .emacs_*

