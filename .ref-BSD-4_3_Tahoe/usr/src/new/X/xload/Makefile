#	Copyright 1985, Massachusetts Institute of Technology.
#
#	xload - makefile for xload 
#

DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O $(INCLUDES)
XLIB = ../Xlib/libX.a
SRCS = xload.c
OBJS = xload.o
PROGRAM = xload

all: xload

clean: 
	rm -f *.bak *.o *~ $(PROGRAM) \#*

# note that on many systems, /dev/kmem is not readable except by a special group
# group kmem is the name of this group on 4.3.
install: all
	install -s -m 2755 -g kmem xload ${DESTDIR}${CONFDIR}

xload: xload.o ../Xlib/Xlib.h
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB)

$(OBJS): $(SRCS)

