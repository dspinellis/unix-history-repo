#
# Copyright (c) 1987 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.7 (Berkeley) %G%
#
CFLAGS=	-O -DCM_N -DCM_GT -DCM_B -DCM_D
LIBC=	/lib/libc.a
SRCS=	termcap.c tgoto.c tputs.c
OBJS=	termcap.o tgoto.o tputs.o
MAN=	termcap.0
MDIR=	${DESTDIR}/usr/man/cat3

.c.o:
	${CC} ${CFLAGS} -c -p $*.c
	ld -x -r -o profiled/$*.o $*.o
	${CC} ${CFLAGS} -c $*.c
	ld -X -r $*.o
	mv a.out $*.o

all: termcap.a termcap_p.a

termcap.a termcap_p.a: ${OBJS}
	@echo building normal termcap
	@ar cr termcap.a ${OBJS}
	ranlib termcap.a
	@echo building profiled termcap
	@cd profiled; ar cr ../termcap_p.a ${OBJS}
	ranlib termcap_p.a

test:
	${CC} ${CFLAGS} -o tc1 tc1.c termcap.a
	${CC} ${CFLAGS} -o tc2 tc2.c termcap.a
	${CC} ${CFLAGS} -o tc3 tc3.c termcap.a

clean:
	rm -f ${OBJS} core profiled/*.o termcap.a termcap_p.a

cleandir: clean
	rm -f ${MAN} tags .depend

depend:
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -o bin -g bin -m 644 termcap.a ${DESTDIR}/usr/lib/libtermcap.a
	ranlib -t ${DESTDIR}/usr/lib/libtermcap.a
	rm -f ${DESTDIR}/usr/lib/libtermlib.a
	ln ${DESTDIR}/usr/lib/libtermcap.a ${DESTDIR}/usr/lib/libtermlib.a
	install -o bin -g bin -m 644 termcap_p.a \
	    ${DESTDIR}/usr/lib/libtermcap_p.a
	ranlib -t ${DESTDIR}/urs/lib/libtermcap_p.a
	rm -f ${DESTDIR}/usr/lib/libtermlib_p.a
	ln ${DESTDIR}/usr/lib/libtermcap_p.a ${DESTDIR}/usr/lib/libtermlib_p.a
	install -c -o bin -g bin -m 444 termcap.0 ${DESTDIR}/usr/man/cat3
	rm -f ${MDIR}/tgetent.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetent.0
	rm -f ${MDIR}/tgetflag.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetflag.0
	rm -f ${MDIR}/tgetnum.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetnum.0
	rm -f ${MDIR}/tgetstr.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetstr.0
	rm -f ${MDIR}/tgoto.0; ln ${MDIR}/termcap.0 ${MDIR}/tgoto.0
	rm -f ${MDIR}/tputs.0; ln ${MDIR}/termcap.0 ${MDIR}/tputs.0

lint:
	lint ${CFLAGS} ${SRCS}

tags:
	ctags ${SRCS}
