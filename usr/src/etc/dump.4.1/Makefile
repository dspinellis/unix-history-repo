#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) 6/5/85
#
#	dump.h			header file
#	dumpitime.c		reads /etc/ddate
#	dumpmain.c		driver
#	dumpoptr.c		operator interface
#	dumptape.c		handles the mag tape and opening/closing
#	dumptraverse.c		traverses the file system
#	unctime.c		undo ctime
#	l3tol.c			3-byte to 4-byte unpacking
#
#	DEBUG			use local directory to find ddate and dumpdates
#	TDEBUG			trace out the process forking
#
PSRCS = \
	dump.h dumpmain.c dumptraverse.c dumptape.c dumpoptr.c dumpitime.c

SRCS =  \
	dump.h dumpitime.c \
	dumpmain.c dumpoptr.c dumptape.c \
	dumptraverse.c unctime.c l3tol.c

OBJS = \
	dumpitime.o \
	dumpmain.o dumpoptr.o \
	dumptape.o dumptraverse.o unctime.o l3tol.o

DFLAGS =
CFLAGS = -O $(DFLAGS)

dump:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o dump
install:
	install -s dump $(DESTDIR)/etc/dump.4.1
clean:
	rm -f *.o dump

lint:
	lint $(DFLAGS) $(SRCS)

psrcs:
	echo $(PSRCS)
