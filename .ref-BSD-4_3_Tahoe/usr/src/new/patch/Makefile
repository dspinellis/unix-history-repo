#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.7 (Berkeley) 7/9/88
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
