# Copyright 1985, Massachusetts Institute of Technology
#	xwud - makefile for the Athena X window system window
#		   window raster image undumper.
#
#		Written by:	Tony Della Fera, DEC
#

DESTDIR =
INCLUDES = -I../include

CONFDIR = /usr/new
XLIB = ../Xlib/libX.a
XH = ../Xlib/Xlib.h

CFLAGS = -O $(INCLUDES)
CLIBS =

.SUFFIXES: .o .h .c .a

OBJS = xwud.o

all: xwud

xwud: $(OBJS)
	$(CC) $(CFLAGS) -o xwud $(OBJS) $(XLIB) $(CLIBS)

xwud.o: ../include/X/XWDFile.h $(XH)

link:
	$(CC) $(CFLAGS) -o xwud $(OBJS) $(XLIB) $(CLIBS)

install: all
	install -s xwud $(DESTDIR)$(CONFDIR)
	chmod 755 $(DESTDIR)$(CONFDIR)/xwud

clean: 
	rm -f *~* *.bak core \#*
	rm -f xwud.c.[0-9]* Makefile.[0-9]* xwud.1.[0-9]*
	rm -f xwud.o xwud

igrind:
	igrind xwud.c
