#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.6 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	patch.c pch.c inp.c version.c util.c
OBJS=	patch.o pch.o inp.o util.o version.o

.SUFFIXES: .man .0

.man.0:
	${DESTDIR}/usr/man/manroff $*.man > $*.0

all: patch

patch: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core patch Makefile.old bsd config.sh \
	eunice loc pdp11 usg v7

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: patch.0
	install -c -o bin -g bin -m 444 patch.0 ${DESTDIR}/usr/new/man/cat1
	install -s -o bin -g bin -m 755 patch ${DESTDIR}/usr/new

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

patch.o: patch.c INTERN.h common.h config.h /usr/include/stdio.h
patch.o: /usr/include/assert.h /usr/include/sys/types.h /usr/include/sys/stat.h
patch.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/machine/trap.h
patch.o: EXTERN.h version.h util.h pch.h inp.h
pch.o: pch.c EXTERN.h common.h config.h /usr/include/stdio.h
pch.o: /usr/include/assert.h /usr/include/sys/types.h /usr/include/sys/stat.h
pch.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/machine/trap.h
pch.o: util.h INTERN.h pch.h
inp.o: inp.c EXTERN.h common.h config.h /usr/include/stdio.h
inp.o: /usr/include/assert.h /usr/include/sys/types.h /usr/include/sys/stat.h
inp.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/machine/trap.h
inp.o: util.h pch.h INTERN.h inp.h
version.o: version.c EXTERN.h common.h config.h /usr/include/stdio.h
version.o: /usr/include/assert.h /usr/include/sys/types.h
version.o: /usr/include/sys/stat.h /usr/include/ctype.h /usr/include/signal.h
version.o: /usr/include/machine/trap.h util.h INTERN.h patchlevel.h version.h
util.o: util.c EXTERN.h common.h config.h /usr/include/stdio.h
util.o: /usr/include/assert.h /usr/include/sys/types.h /usr/include/sys/stat.h
util.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/machine/trap.h
util.o: INTERN.h util.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
