#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
CFLAGS=	-DHERE=TAHOE -DTARGET=TAHOE -DFAMILY=PCC -DUCBVAXASM \
	-DUCBPASS2 -O -I$(INCLDIR)/usr/include -I../include
LIBC=	/lib/libc.a
SRCS=	f77.c
OBJS=	f77.o

all: f77

f77: ${SRCS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS}

clean:
	rm -f ${OBJS} core f77

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 f77 ${DESTDIR}/usr/bin

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
