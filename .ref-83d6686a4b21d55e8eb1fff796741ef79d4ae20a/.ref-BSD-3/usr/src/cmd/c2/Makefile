DESTDIR = 
CFLAGS=-O -d2 -w

c2:	c20.o c21.o c22.o
	$(CC) -o c2 c2?.o

mon.o: /usr/lib/mon.c
	$(CC) -c -p -DMASTER='"/usr/lib/c2.monsum"' -DINCREMENT='"/tmp/c2XXXXX"' /usr/lib/mon.c
	
c20.o:	c20.c c2.h
c21.o:	c21.c c2.h
c22.o:  c22.c c2.h
	$(CC) $(CFLAGS) -S c22.c
	/usr/src/cmd/as/:rofix c22.s
	as -o c22.o c22.s

clean :
	rm -f *.o *.s

install :
	install -s c2 $(DESTDIR)/lib
