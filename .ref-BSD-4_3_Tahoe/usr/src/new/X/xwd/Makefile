# Copyright, 1985 Massachusetts Institute of Technology
#
#	xwd - makefile for the Athena X window system window
#		   window raster image dumper.
#
#		Written by:	Tony Della Fera, DEC
#				10-May-85


DESTDIR =
INCLUDES = -I../include

CURSDIR = ../cursors
CONFDIR = /usr/new
XLIB = ../Xlib/libX.a
XH = ../Xlib/Xlib.h
CFLAGS = -O $(INCLUDES)
CLIBS =

.SUFFIXES: .o .h .c .a

OBJS = xwd.o

all: xwd

xwd: $(OBJS) $(XLIB)
	$(CC) $(CFLAGS) -o xwd $(OBJS) $(XLIB) $(CLIBS)

xwd.o: XWDFile.h \
	$(CURSDIR)/target.cursor $(CURSDIR)/target_mask.cursor \
	$(XH)

link:
	$(CC) $(CFLAGS) -o xwd $(OBJS) $(XLIB) $(CLIBS)

install: all
	install -s xwd $(DESTDIR)$(CONFDIR)
	chmod 755 $(DESTDIR)$(CONFDIR)/xwd

clean: 
	rm -f *~* *.bak core xwd.o xwd \#*
	rm -f xwd.c.[0-9]* Makefile.[0-9]* xwd.1.[0-9]*

vgrind:
	vgrind xwd.c
