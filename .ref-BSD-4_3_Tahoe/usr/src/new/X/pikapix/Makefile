# Copyright, 1985 Massachusetts Institute of Technology

DESTDIR=
INCLUDES= -I../include

CONFDIR= /usr/new
XLIB = ../Xlib/libX.a
CFLAGS = -O ${INCLUDES}

OBJS = pikapix.o

all: 	pikapix

pikapix.o: ../Xlib/Xlib.h

pikapix: $(OBJS) $(XLIB)
	$(CC) $(CFLAGS) -o pikapix $(OBJS) $(XLIB)

install: all
	install -c pikapix $(DESTDIR)$(CONFDIR)
	chmod 755 $(DESTDIR)$(CONFDIR)/pikapix
clean:
	rm -f pikapix *.o *.bak *~ \#*
