#	Copyright 1985, Massachusetts Institute of Technology
#	bitmap - makefile for bitmap

DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O ${INCLUDES}
XLIB = ../Xlib/libX.a
SRCS = bitmap.c dialog.c
OBJS = bitmap.o dialog.o
PROGRAM = bitmap

all: bitmap

clean: 
	rm -f ${PROGRAM} *.bak *.o core errs \#* *~

install: all
	install bitmap ${DESTDIR}${CONFDIR}

bitmap: $(OBJS) $(XLIB)
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB) -lm

bitmap.o: ../Xlib/Xlib.h
dialog.o: ../Xlib/Xlib.h

