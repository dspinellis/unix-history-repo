# Copyright 1985, Massachusetts Institute of Technology
#
#	xwininfo - makefile for the Athena X window system window
#		   information utility.
#
#		Written by:	Tony Della Fera, DEC
#				10-May-85
#

DESTDIR =
INCLUDES = -I../include

CONFDIR = /usr/new
XLIB = ../Xlib/libX.a
XH = ../Xlib/Xlib.h
CFLAGS = -O $(INCLUDES)
CLIBS =

.SUFFIXES: .o .h .c .a

OBJS = xwininfo.o

all: xwininfo

xwininfo: $(OBJS) $(XLIB)
	$(CC) $(CFLAGS) -O -o xwininfo $(OBJS) $(XLIB) $(CLIBS)

xwininfo.o: ../cursors/target.cursor ../cursors/target_mask.cursor $(XH)

link:
	$(CC) $(CFLAGS) -O -o xwininfo $(OBJS) $(XLIB) $(CLIBS)


install: all
	install xwininfo $(DESTDIR)$(CONFDIR)

clean: 
	rm -f *~* *.bak core \#* xwininfo.o xwininfo
	rm -f xwininfo.c.[0-9]* Makefile.[0-9]* xwininfo.1.[0-9]*

igrind:
	igrind xwininfo.c

