#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	6/2/87
#

# If you want something other than Eastern United States time used on your
# system, change the line below (after finding the zone you want in the
# time zone files, or adding it to a time zone file).
# Alternately, if you discover you've got the wrong time zone, you can just
#	zic -l rightzone
LOCALTIME=	US/Pacific

# If you want code inspired by certain emerging standards, add
#	-DSTD_INSPIRED
# to the end of the "CFLAGS=" line.

CFLAGS=	-O
LIBC=	/lib/libc.a
TZCSRCS=	zic.c scheck.c ialloc.c
TZCOBJS=	zic.o scheck.o ialloc.o
TZDSRCS=	zdump.c ialloc.c
TZDOBJS=	zdump.o ialloc.o
DOCS=		Theory README Makefile newctime.3 tzfile.5 zic.8 zdump.8
SRCS=		zic.c zdump.c scheck.c ialloc.c

# If you want to handle solar-time-based time zones, remove the
# "#define NOSOLAR" from the include file usr/include/tzfile.h.
# (and add solar87 to the DATA= line below).
DATA=		asia australasia europe etcetera northamerica \
		pacificnew systemv

all: zdump zic

zdump: ${TZDOBJS} ${LIBC}
	${CC} ${CFLAGS} ${TZDOBJS} -o $@

zic: ${TZCOBJS} ${LIBC}
	${CC} ${CFLAGS} ${TZCOBJS} -o $@

clean: FRC
	rm -f ${TZDOBJS} ${TZCOBJS} core zdump zic

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	./zic ${DATA}
	./zic -d ${DESTDIR}/etc/zoneinfo -l ${LOCALTIME} ${DATA}

lint: FRC
	lint ${CFLAGS} ${TZCSRCS}
	lint ${CFLAGS} ${TZDSRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

zic.o: zic.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
zic.o: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/strings.h
zic.o: /usr/include/time.h /usr/include/tzfile.h
zdump.o: zdump.c /usr/include/stdio.h /usr/include/sys/types.h
zdump.o: /usr/include/tzfile.h /usr/include/time.h
scheck.o: scheck.c /usr/include/stdio.h /usr/include/ctype.h
ialloc.o: ialloc.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
