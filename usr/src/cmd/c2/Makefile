CFLAGS=-c -O -d2 -w -DCOPYCODE -g
LDFLAGS=-g

c2:	c20.o c21.o c22.o
	$(CC) $(LDFLAGS) -z -o c2 c2?.o

c20.o:	c20.c c2.h
c21.o:	c21.c c2.h
c22.o:  c22.c c2.h
	$(CC) $(CFLAGS) -R c22.c

clean:
	rm -f *.o *.s 

install:
	install -s c2 $(DESTDIR)/lib
