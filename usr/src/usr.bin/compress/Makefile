#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.8	(Berkeley)	%G%
#
CFLAGS=	-O -DBSD4_2 -DSACREDMEM=256000 -DUSERMEM=`cat USERMEM` 
LIBC=	/lib/libc.a
SRCS=	compress.c
OBJS=	compress.o

all: compress

compress: USERMEM ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

# USERMEM may have to be set by hand.  It should contain the amount of
# available user memory in bytes.  Set it to zero, for physical memory
# less than 1 Meg.
USERMEM:
	sh usermem.sh > USERMEM

# Temporarily don't delete USERMEM.  When chroot'ed to /nbsd, usermem.sh
# fails totally.
clean: FRC
	rm -f ${OBJS} core compress
#	rm -f ${OBJS} core compress USERMEM

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 compress ${DESTDIR}/usr/ucb/compress
	rm -f ${DESTDIR}/usr/ucb/uncompress ${DESTDIR}/usr/ucb/zcat
	ln ${DESTDIR}/usr/ucb/compress ${DESTDIR}/usr/ucb/uncompress
	ln ${DESTDIR}/usr/ucb/compress ${DESTDIR}/usr/ucb/zcat

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

compress.o: compress.c /usr/include/stdio.h /usr/include/ctype.h
compress.o: /usr/include/signal.h /usr/include/sys/types.h
compress.o: /usr/include/sys/stat.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
