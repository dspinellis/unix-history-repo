#
#	 - makefile for xfd
#

DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O $(INCLUDES)
XLIB = ../Xlib/libX.a
SRCS = xfd.c
OBJS = xfd.o
PROGRAM = xfd

all: xfd

clean: 
	rm -f *.bak *.o *~ $(PROGRAM)

install: all
	install xfd ${DESTDIR}${CONFDIR}

xfd: xfd.o ../Xlib/Xlib.h
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB)

$(OBJS): $(SRCS)

