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
#	@(#)Makefile	5.13 (Berkeley) 6/27/88
#
DEFS=	-DLIBC_SCCS
FLAGS=	-I../../tahoe ${DEFS}
CFLAGS=	-O ${FLAGS}
CSRCS=	frexp.c
COBJS=	frexp.o
ASRCS=	_setjmp.s abort.s abs.s alloca.s atof.s bcmp.s bcopy.s bzero.s fabs.s \
	ffs.s insque.s ldexp.s modf.s nargs.s remque.s setjmp.s strcat.s \
	strcmp.s strcpy.s strlen.s strncat.s strncmp.s strncpy.s udiv.s \
	urem.s
AOBJS=	_setjmp.o abort.o abs.o alloca.o atof.o bcmp.o bcopy.o bzero.o fabs.o \
	ffs.o insque.o ldexp.o modf.o nargs.o remque.o setjmp.o strcat.o \
	strcmp.o strcpy.o strlen.o strncat.o strncmp.o strncpy.o udiv.o \
	urem.o
SRCS=	${CSRCS} ${ASRCS}
OBJS=	${COBJS} ${AOBJS}
TAGSFILE=tags

.s.o:
	@/lib/cpp -E ${FLAGS} -DPROF $*.s | ${AS} -o $*.o
	@ld -X -o profiled/$*.o -r $*.o
	/lib/cpp -E ${FLAGS} $*.s | ${AS} -o $*.o
	@ld -x -r $*.o
	@mv a.out $*.o

.c.o:
	@${CC} -pg -DPROF ${CFLAGS} -c $*.c
	@ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

link: ${OBJS}
	(cd ../../library; rm -f ${OBJS})
	(cd ../../profiled; rm -f ${OBJS})
	ln ${OBJS} ../../library
	(cd profiled; ln ${OBJS} ../../../profiled)

tags:
	cwd=`pwd`; \
	for i in ${CSRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done
	(P=`pwd`; \
	egrep "^ENTRY(.*)|^SYSCALL(.*)" ${ASRCS} | sed \
"s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 $$P/\1 /^\2(\3\4$$/;" \
		>> ${TAGSFILE})

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

frexp.o: frexp.c
_setjmp.o: _setjmp.s ../../tahoe/DEFS.h
abort.o: abort.s ../../tahoe/DEFS.h
abs.o: abs.s ../../tahoe/DEFS.h
alloca.o: alloca.s ../../tahoe/DEFS.h
atof.o: atof.s ../../tahoe/DEFS.h
bcmp.o: bcmp.s ../../tahoe/DEFS.h
bcopy.o: bcopy.s ../../tahoe/DEFS.h
bzero.o: bzero.s ../../tahoe/DEFS.h
fabs.o: fabs.s ../../tahoe/DEFS.h
ffs.o: ffs.s ../../tahoe/DEFS.h
insque.o: insque.s ../../tahoe/DEFS.h
ldexp.o: ldexp.s /usr/include/errno.h /usr/include/tahoemath/fp.h
ldexp.o: ../../tahoe/DEFS.h
modf.o: modf.s ../../tahoe/DEFS.h
nargs.o: nargs.s ../../tahoe/DEFS.h
remque.o: remque.s ../../tahoe/DEFS.h
setjmp.o: setjmp.s ../../tahoe/DEFS.h
strcat.o: strcat.s ../../tahoe/DEFS.h
strcmp.o: strcmp.s ../../tahoe/DEFS.h
strcpy.o: strcpy.s ../../tahoe/DEFS.h
strlen.o: strlen.s ../../tahoe/DEFS.h
strncat.o: strncat.s ../../tahoe/DEFS.h
strncmp.o: strncmp.s ../../tahoe/DEFS.h
strncpy.o: strncpy.s ../../tahoe/DEFS.h
udiv.o: udiv.s ../../tahoe/DEFS.h
urem.o: urem.s ../../tahoe/DEFS.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
