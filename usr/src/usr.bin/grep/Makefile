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
#	@(#)Makefile	5.1 (Berkeley) %G%
#
# -DEGREPSTD=path	location of std egrep (normally /usr/bin/egrep).
# -DGREPSTD=path	location of std grep (normally /bin/grep).
# -DFGREPSTD=path	location of std fgrep (normally /usr/bin/fgrep).
# -Dstrrchr=rindex, -Dstrchr=index	for troglodytes.
# -DSLOWSYS		invoke xread() for system time quirk on PDP, others? 
# -DNOKANJI		default is for Japanese Unix.  undef only for raw
#			 parity-marked search capability, not standard w/grep.
# -DCHINESE		for systems using EUC Chinese2 codes

CFLAGS=	-g -Dstrrchr=rindex -Dstrchr=index -DNOKANJI \
	-DEGREPSTD=\"/usr/lib/old.egrep\" -DGREPSTD=\"/usr/lib/old.bin.grep\" \
	-DFGREPSTD=\"/usr/lib/old.fgrep\"
LIBC=	/lib/libc.a
ALL=	egrep old.ucb.grep old.bin.grep old.fgrep old.egrep
SRCS=	egrep.c old.ucb.grep.c old.bin.grep.c old.fgrep.c old.egrep.c
OBJS=

all: ${ALL}

egrep: egrep.c ${LIBC}
	${CC} -o $@ ${CFLAGS} egrep.c

oegrep: oegrep.c ${LIBC}
	${CC} -o $@ ${CFLAGS} oegrep.c

old.ucb.grep: ${LIBC}
	${CC} -o $@ ${CFLAGS} old.ucb.grep.c

old.bin.grep: ${LIBC}
	${CC} -o $@ ${CFLAGS} old.bin.grep.c

old.fgrep: ${LIBC}
	${CC} -o $@ ${CFLAGS} old.fgrep.c

old.egrep: ${LIBC}
	${CC} -o $@ ${CFLAGS} old.egrep.c

clean: FRC
	rm -f ${OBJS} core ${ALL} old.egrep.c

depend: FRC ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 egrep ${DESTDIR}/usr/ucb/egrep
	rm -f ${DESTDIR}/usr/ucb/fgrep ${DESTDIR}/usr/ucb/grep
	ln ${DESTDIR}/usr/ucb/egrep ${DESTDIR}/usr/ucb/grep
	ln ${DESTDIR}/usr/ucb/egrep ${DESTDIR}/usr/ucb/fgrep
	install -s -o bin -g bin -m 755 old.bin.grep ${DESTDIR}/usr/lib/old.bin.grep
	install -s -o bin -g bin -m 755 old.ucb.grep ${DESTDIR}/usr/lib/old.ucb.grep
	install -s -o bin -g bin -m 755 old.fgrep ${DESTDIR}/usr/lib/old.fgrep
	install -s -o bin -g bin -m 755 old.egrep ${DESTDIR}/usr/lib/old.egrep

lint: FRC ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: FRC ${SRCS}
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

egrep: egrep.c /usr/include/stdio.h /usr/include/ctype.h
egrep: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/file.h
egrep: /usr/include/regexp.h
old.ucb.grep: old.ucb.grep.c /usr/include/stdio.h /usr/include/sys/types.h
old.ucb.grep: /usr/include/sys/stat.h
old.bin.grep: old.bin.grep.c /usr/include/sys/types.h /usr/include/sys/file.h
old.bin.grep: /usr/include/sys/stat.h /usr/include/stdio.h /usr/include/ctype.h
old.fgrep: old.fgrep.c /usr/include/stdio.h /usr/include/ctype.h
old.fgrep: /usr/include/sys/param.h /usr/include/sys/types.h
old.fgrep: /usr/include/signal.h /usr/include/machine/trap.h
old.fgrep: /usr/include/machine/machparam.h /usr/include/sys/stat.h
old.egrep: old.egrep.c /usr/include/stdio.h /usr/include/sys/types.h
old.egrep: /usr/include/sys/stat.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
