#	Copyright 1986, Massachusetts Institute of Technology.
#
#	xcolors - makefile for xcolors
#

DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O $(INCLUDES)
XLIB = ../Xlib/libX.a
SRCS = xcolors.c
OBJS = xcolors.o
PROGRAM = xcolors

all: xcolors

clean: 
	rm -f *.bak *.o *~ $(PROGRAM) \#*

install: all
	install -c ${PROGRAM} ${DESTDIR}${CONFDIR}

${PROGRAM}: ${OBJS} ../Xlib/Xlib.h
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB)

$(OBJS): $(SRCS)

