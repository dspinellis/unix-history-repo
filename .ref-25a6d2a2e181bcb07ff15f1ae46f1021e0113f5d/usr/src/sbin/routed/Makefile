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
#	@(#)Makefile	5.9 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
C2=	/usr/libexec/c2
INLINE=	/usr/include/${MACHINE}/inline/inline
SRCS=	af.c if.c input.c main.c output.c startup.c tables.c timer.c \
	trace.c inet.c
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o inet.o
HDRS=	af.h defs.h interface.h table.h trace.h
MAN=	routed.0

all: routed

routed: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

# The following can be deleted where not appropriate to use the kernel's
# inline code expansions.
.c.o:
	${CC} -S ${CFLAGS} $*.c
	${C2} $*.s | ${INLINE} | ${AS} -o $*.o
	rm -f $*.s

clean:
	rm -f ${OBJS} core routed
	(cd tools; make ${MFLAGS} clean)

cleandir: clean
	rm -f ${MAN} tags .depend
	(cd tools; make ${MFLAGS} cleandir)

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}
	(cd tools; make ${MFLAGS} depend)

install: ${MAN}
	install -s -o bin -g bin -m 755 routed ${DESTDIR}/usr/sbin
	install -c -o bin -g bin -m 444 routed.0 ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${HDRS} ${SRCS}
	(cd tools; make ${MFLAGS} tags)
