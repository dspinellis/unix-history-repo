#
# Copyright, 1985, Massachusetts Institute of Technology.
#	xclock - makefile for the X window system clock.
#
#	Written by:	Tony Della Fera, DEC
#			11-Sep-84
#

DESTDIR =
INCLUDES = -I../include

CONFDIR = /usr/new
XLIB = ../Xlib/libX.a
CFLAGS = -O $(INCLUDES)
CLIBS = -lm

.SUFFIXES: .o .h .c .a

OBJS = xclock.o

all: xclock

debug: xclock_d

install: all
	install -s xclock $(DESTDIR)$(CONFDIR)
	chmod 755 $(DESTDIR)$(CONFDIR)/xclock

clean: 
	rm -f *~* *.bak core \#*
	rm -f xclock.c.[0-9]* Makefile.[0-9]* xclock.1.[0-9]*
	rm -f xclock.o xclock xclock_d

igrind:
	igrind xclock.c

xclock: $(OBJS) $(XLIB)
	$(CC) $(CFLAGS) -o xclock $(OBJS) $(XLIB) $(CLIBS)

xclock_d: $(OBJS) $(XLIB)
	$(CC) $(CFLAGS) -g -o xclock_d $(OBJS) $(XLIB) $(CLIBS)
