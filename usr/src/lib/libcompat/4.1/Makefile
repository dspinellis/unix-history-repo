#
# Copyright (c) 1988 Regents of the University of California.
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
#	@(#)Makefile	5.11 (Berkeley) %G%
#
DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}

STDSRCS=ftime.c gtty.c nice.c pause.c rand.c stty.c tell.c times.c \
	utime.c vlimit.c vtimes.c
STDOBJS=ftime.o gtty.o nice.o pause.o rand.o stty.o tell.o times.o \
	utime.o vlimit.o vtimes.o

# these should just be pitched, but in the interest of compatibility...
TRASHSRCS=getpw.c
TRASHOBJS=getpw.o
SRCS=	${STDSRCS} ${TRASHSRCS}
OBJS=	${STDOBJS} ${TRASHOBJS}
TAGSFILE=tags

.c.o:
	@${CC} -p ${CFLAGS} -c $*.c
	@-ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

link: ${OBJS}
	(cd ../library; rm -f ${OBJS})
	(cd ../profiled; rm -f ${OBJS})
	ln ${OBJS} ../library
	(cd profiled; ln ${OBJS} ../../profiled)

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ftime.o: ftime.c /usr/include/sys/types.h /usr/include/sys/time.h
ftime.o: /usr/include/time.h
gtty.o: gtty.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
gtty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
gtty.o: /usr/include/sys/ttydev.h
nice.o: nice.c /usr/include/sys/time.h /usr/include/time.h
nice.o: /usr/include/sys/resource.h
pause.o: pause.c
rand.o: rand.c
stty.o: stty.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
stty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
stty.o: /usr/include/sys/ttydev.h
tell.o: tell.c
times.o: times.c /usr/include/sys/time.h /usr/include/time.h
times.o: /usr/include/sys/resource.h
utime.o: utime.c /usr/include/sys/time.h /usr/include/time.h
vlimit.o: vlimit.c /usr/include/sys/time.h /usr/include/time.h
vlimit.o: /usr/include/sys/resource.h /usr/include/errno.h
vtimes.o: vtimes.c /usr/include/sys/time.h /usr/include/time.h
vtimes.o: /usr/include/sys/resource.h
getpw.o: getpw.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
