#	Copyright 1985, Massachusetts Institute of Technology.

DESTDIR=
CONFDIR= /usr/new
INCLUDES= -I../../include

CFLAGS = -O $(INCLUDES)
XLIB = ../../Xlib/libX.a
OBJS = maze.o

all: 	maze

maze: $(OBJS)
	$(CC) $(CFLAGS) -o maze $(OBJS) $(XLIB)

install: all
	install -c -s -m 755 maze $(DESTDIR)$(CONFDIR)

clean:
	rm -f maze *.o *.bak *~ \#*
