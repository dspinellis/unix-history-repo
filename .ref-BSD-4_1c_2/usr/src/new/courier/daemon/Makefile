OBJS = courierd.o
CFLAGS =  -O
LIBS = -ljobs
DESTDIR = /etc

courierd:	$(OBJS)
	$(CC) $(CFLAGS) -o courierd $(OBJS) $(LIBS)

install:	courierd
	install -s courierd $(DESTDIR)
