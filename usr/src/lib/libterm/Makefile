#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
#
CFLAGS=	-O -DCM_N -DCM_GT -DCM_B -DCM_D
LIBC=	/lib/libc.a
SRCS=	termcap.c tgoto.c tputs.c
OBJS=	termcap.o tgoto.o tputs.o

.c.o:
	${CC} ${CFLAGS} -c -p $*.c
	ld -x -r -o profiled/$*.o $*.o
	${CC} ${CFLAGS} -c $*.c
	ld -X -r $*.o
	mv a.out $*.o

all: termcap.a termcap_p.a

termcap.a termcap_p.a: ${OBJS}
	ar cr termcap.a ${OBJS}
	cd profiled; ar cr ../termcap_p.a ${OBJS}

clean: FRC
	rm -f ${OBJS} core profiled/*.o termcap.a termcap_p.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 termcap.a ${DESTDIR}/usr/lib/libtermcap.a
	rm -f ${DESTDIR}/usr/lib/libtermlib.a
	ln ${DESTDIR}/usr/lib/libtermcap.a ${DESTDIR}/usr/lib/libtermlib.a
	ranlib ${DESTDIR}/usr/lib/libtermcap.a
	install -o bin -g bin -m 644 termcap_p.a ${DESTDIR}/usr/lib/libtermcap_p.a
	rm -f ${DESTDIR}/usr/lib/libtermlib_p.a
	ln ${DESTDIR}/usr/lib/libtermcap_p.a ${DESTDIR}/usr/lib/libtermlib_p.a
	ranlib ${DESTDIR}/usr/lib/libtermcap_p.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

termcap.o: termcap.c /usr/include/ctype.h
tgoto.o: tgoto.c
tputs.o: tputs.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
tputs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tputs.o: /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
