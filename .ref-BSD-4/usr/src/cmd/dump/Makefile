# sccsid = "@(#)Makefile	1.2 (Berkeley) 10/18/80"
#
#	dump.h			header file
#	dumpitime.c		reads /etc/ddate
#	dumpmain.c		driver
#	dumpoptr.c		operator interface
#	dumptape.c		handles the mag tape and opening/closing
#	dumptraverse.c		traverses the file system
#	unctime.c		undo ctime
#
#	DEBUG			use local directory to find ddate and dumpdates
#	TDEBUG			trace out the process forking
#
PSRCS = \
	dump.h dumpmain.c dumptraverse.c dumptape.c dumpoptr.c dumpitime.c

SRCS =  \
	dump.h dumpitime.c \
	dumpmain.c dumpoptr.c dumptape.c \
	dumptraverse.c unctime.c

OBJS = \
	dumpitime.o \
	dumpmain.o dumpoptr.o \
	dumptape.o dumptraverse.o unctime.o

DFLAGS = -DERNIE
CFLAGS = -O $(DFLAGS)

dump:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o dump
install:
	install -s dump $(DESTDIR)/etc
clean:
	rm -f *.o dump

lint:
	lint $(DFLAGS) $(SRCS)

psrcs:
	echo $(PSRCS)
