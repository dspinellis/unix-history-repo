CFLAGS = -O
USEROBJS1 = datime.o implicit.o
USEROBJS2 = timex.o explicit.o
SRVROBJS = Time.o Time_server.o
LIBS = -lcr
DESTDIR = /usr/new/lib/courier

all:	datime timex Time

datime:	$(USEROBJS1)
	cc -o datime $(USEROBJS1) $(LIBS)

implicit.o:	Time.cr
	courier Time.cr
	cc -c $(CFLAGS) Time_client.c
	mv Time_client.o implicit.o

timex:	$(USEROBJS2)
	cc -o timex $(USEROBJS2) $(LIBS)

explicit.o:	Time.cr
	courier -x Time.cr
	cc -c $(CFLAGS) Time_client.c
	mv Time_client.o explicit.o

Time:	$(SRVROBJS)
	cc -o Time $(SRVROBJS) $(LIBS)

datime.o timex.o Time.o:	Time.h

Time_server.c Time.h:	Time.cr
	courier Time.cr

install:	all
	install -s Time $(DESTDIR)

clean:
	-rm -f *.o Time_*.c Time.h \#* Time datime timex
