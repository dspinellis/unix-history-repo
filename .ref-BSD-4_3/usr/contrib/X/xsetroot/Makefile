# Copyright 1985, Massachusetts Institute of Technology
#
#	xsetroot - makefile for the Athena X window system window 
#		   root window parameter setting utility
#
#		Written by:	Tony Della Fera, DEC
#				4-Oct-85
#

DESTDIR =
INCLUDES = -I../include

CONFDIR = /usr/new
XLIB = ../Xlib/libX.a
CFLAGS = -O $(INCLUDES)
CLIBS =

.SUFFIXES: .o .h .c .a

OBJS = xsetroot.o

all: xsetroot

xsetroot: $(OBJS) ../Xlib/Xlib.h
	$(CC) $(CFLAGS) -o xsetroot $(OBJS) $(XLIB) $(CLIBS)

link:
	$(CC) $(CFLAGS) -g -o xsetroot $(OBJS) $(XLIB) $(CLIBS)


install: all
	install -s xsetroot ${DESTDIR}${CONFDIR}

clean:
	rm -f *~* *.bak core \#* xsetroot.o xsetroot
	rm -f xsetroot.c.[0-9]* Makefile.[0-9]* xsetroot.1.[0-9]*

igrind:
	igrind xsetroot.c

consistent:
	ci -l -m"Consistency check in." *.[ch] Makefile
