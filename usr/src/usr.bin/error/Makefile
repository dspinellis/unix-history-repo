#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
#
#	error.h			error header file
#	errorinput.c		does all of the input, and canonicalizes errors
#	errorpi.c		canonicalizes pi errors
#	errormain.c		main
#	errorsubr.c		does grotty subroutine work
#	errorfilter.c		"greps" out unwanted messages
#	errortouch.c		touches all mentioned files
#

HDRS = error.h
PHDRS = $(HDRS)
SRCS = errormain.c errorinput.c errorpi.c errorsubr.c errorfilter.c errortouch.c
PSRCS = $(SRCS)
OBJS = errormain.o errorinput.o errorpi.o errorsubr.o errorfilter.o errortouch.o

DFLAGS = -DDEBUG
CFLAGS =  -O $(DFLAGS)

LINTFLAGS = $(DFLAGS)

error:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o error

$(OBJS): $(HDRS)

install:
	install -s error $(DESTDIR)/usr/ucb

clean:
	rm -f error $(OBJS)

lint:	
	lint $(LINTFLAGS) $(SRCS)

psrcs:
	echo $(PHDRS) $(PSRCS)

print:
	print $(PHDRS) $(PSRCS)

depend:
