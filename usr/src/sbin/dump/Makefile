# sccsid = "@(#)Makefile	1.5 (Berkeley) %G%"
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
	/usr/include/dumprestor.h dump.h \
	dumpmain.c dumptraverse.c dumptape.c dumpoptr.c dumpitime.c unctime.c

SRCS =  \
	dump.h dumpitime.c \
	dumpmain.c dumpoptr.c dumptape.c \
	dumptraverse.c unctime.c

OBJS = \
	dumpitime.o \
	dumpmain.o dumpoptr.o \
	dumptape.o dumptraverse.o unctime.o

DFLAGS =
CFLAGS = -O -DKERNEL $(DFLAGS)

dump:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o dump

install:
	install -s dump $(DESTDIR)/etc

clean:
	rm -f *.o dump

lint:
	lint $(DFLAGS) $(SRCS)

print:
	pr $(PSRCS) | lpr

$(OBJS): dump.h /usr/include/sys/param.h
$(OBJS): /usr/include/sys/fs.h /usr/include/sys/inode.h
$(OBJS): /usr/include/dumprestor.h /usr/include/stat.h
