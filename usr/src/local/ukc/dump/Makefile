#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 5/28/86
#	@(#)Makefile	1.2 (UKC) %G%
#
#	dump.h			header file
#	dumpitime.c		reads /etc/dumpdates
#	dumpmain.c		driver
#	dumpoptr.c		operator interface
#	dumptape.c		handles the mag tape and opening/closing
#	dumplab.c		deal with tape labels
#	dumptraverse.c		traverses the file system
#	unctime.c		undo ctime
#
#	dumplabel.c		General tape label management program
#
#	DEBUG			use local directory to find ddate and dumpdates
#	TDEBUG			trace out the process forking
#
PSRCS = \
	/usr/include/protocols/dumprestore.h dump.h \
	dumpmain.c dumptraverse.c dumptape.c dumplab.c \
	dumpoptr.c dumpitime.c unctime.c dumplabel.c

SRCS =  \
	dump.h dumpitime.c \
	dumpmain.c dumpoptr.c dumprmt.c dumprtape.c dumplab.c \
	dumptraverse.c unctime.c

OBJS = dumpitime.o dumpmain.o dumpoptr.o dumplab.o \
	dumptape.o dumptraverse.o unctime.o

ROBJS = dumpitime.o dumprmain.o dumpoptr.o dumplab.o \
	dumprmt.o dumprtape.o dumptraverse.o unctime.o

DFLAGS =
CFLAGS = -O $(DFLAGS)

all: dump rdump dumplabel

rdump:	$(ROBJS)
	$(CC) $(CFLAGS) $(ROBJS) -o rdump -lm

dump:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o dump -lm

dumplabel: dumplabel.o
	$(CC) $(CFLAGS) dumplabel.o -o dumplabel

install: rdump dump
	install -s -m 6755 -g tty rdump $(DESTDIR)/etc
	install -s -m 2755 -g tty dump $(DESTDIR)/etc
	install -s -m 755 -g tty $(DESTDIR)/etc

dumprmain.c:
	ln -s dumpmain.c dumprmain.c
dumprmain.o: dumprmain.c
	${CC} -c -DRDUMP ${CFLAGS} dumprmain.c

dumprtape.c:
	ln -s dumptape.c dumprtape.c
dumprtape.o: dumprtape.c
	${CC} -c -DRDUMP ${CFLAGS} dumprtape.c

clean:
	rm -f *.o rdump dump errs dumprmain.c dumprtape.c

lint:
	lint $(DFLAGS) $(SRCS)

print:
	pr $(PSRCS) | lpr

$(OBJS): dump.h /usr/include/sys/param.h
$(OBJS): /usr/include/sys/fs.h /usr/include/sys/inode.h
$(OBJS): /usr/include/protocols/dumprestore.h /usr/include/sys/stat.h
