SRCS = client.c bind.c server.c misc.c
OBJS = client.o bind.o server.o misc.o
CFLAGS = -O -DDEBUG
DESTDIR = /usr/lib

libcr.a:	$(OBJS)
	-rm -f libcr.a
	ar r libcr.a $(OBJS)
	ranlib libcr.a

lint:	$(SRCS)
	lint -hux $(SRCS)

install:	libcr.a
	install -m 644 libcr.a $(DESTDIR)

clean:
	-rm -f *.o
