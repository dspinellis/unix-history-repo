#
# Copyright, 1985, Massachusetts Institute of Technology.
#	xrefresh - trivial program to refresh the screen
#

DESTDIR =
INCLUDES = -I../include

CONFDIR = /usr/new
XLIB = ../Xlib/libX.a
CFLAGS = -O $(INCLUDES)
PROG = xrefresh

OBJS = $(PROG).o

all: $(PROG)

install: all
	install -c $(PROG) $(DESTDIR)$(CONFDIR)
	chmod 755 $(DESTDIR)$(CONFDIR)/$(PROG)

clean: 
	rm -f $(PROG).o $(PROG) *~ \#*

igrind:
	igrind $(PROG).c

$(PROG): $(OBJS) ../Xlib/Xlib.h
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(XLIB)

