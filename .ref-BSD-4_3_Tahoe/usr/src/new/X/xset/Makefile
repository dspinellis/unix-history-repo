# Copyright 1985, Massachusetts Institute of Technology.
DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O $(INCLUDES)
XLIB = ../Xlib/libX.a
SRCS = xset.c
OBJS = xset.o
PROGRAM = xset

.SUFFIXES: .o .h .c .a

####	Maintainance keys    #####

all: $(PROGRAM)

install:
	install xset ${DESTDIR}${CONFDIR}

clean: 
	rm -f xset.o xset
	rm -f *.[0-9]* *~* *.bak core \#*

#####     xset     #####
$(OBJS): $(SRCS)

xset: xset.o ../Xlib/Xlib.h
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB)

