# Copyright 1985, Massachusetts Institute of Technology.
DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O $(INCLUDES)
XLIB = ../Xlib/libX.a
SRCS = xhost.c
OBJS = xhost.o
PROGRAM = xhost

.SUFFIXES: .o .h .c .a

####	Maintainance keys    #####

all: $(PROGRAM)

install:
	install xhost ${DESTDIR}${CONFDIR}

clean: 
	rm -f xhost.o xhost
	rm -f *.[0-9]* *~* *.bak core \#*

#####     xhost     #####
$(OBJS): $(SRCS)

xhost: xhost.o ../Xlib/Xlib.h
	cc $(CFLAGS) $(OBJS) -o $(PROGRAM) $(XLIB)
