# sccsid = "@(#)Makefile	1.10 (Berkeley) %G%"
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
	dumpmain.c dumpoptr.c dumprmt.c dumprtape.c \
	dumptraverse.c unctime.c

OBJS = dumpitime.o dumpmain.o dumpoptr.o \
	dumptape.o dumptraverse.o unctime.o

ROBJS = dumpitime.o dumprmain.o dumpoptr.o \
	dumprmt.o dumprtape.o dumptraverse.o unctime.o

DFLAGS =
CFLAGS = -O $(DFLAGS)

all: dump rdump

rdump:	$(ROBJS)
	$(CC) $(CFLAGS) $(ROBJS) -o rdump -ljobs

dump:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o dump

install: rdump dump
	install -s rdump $(DESTDIR)/etc
	install -s dump $(DESTDIR)/etc

dumprmain.o: dumpmain.c
	cp dumpmain.c dumprmain.c
	${CC} -c -DRDUMP ${CFLAGS} dumprmain.c
	rm -f dumprmain.c

clean:
	rm -f *.o rdump dump errs dumprmain.c

lint:
	lint $(DFLAGS) $(SRCS)

print:
	pr $(PSRCS) | lpr

$(OBJS): dump.h /usr/include/sys/param.h
$(OBJS): /usr/include/sys/fs.h /usr/include/sys/inode.h
$(OBJS): /usr/include/dumprestor.h /usr/include/stat.h
