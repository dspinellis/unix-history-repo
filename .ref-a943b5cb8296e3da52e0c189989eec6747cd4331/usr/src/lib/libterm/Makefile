#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
CFLAGS=	-O -DCM_N -DCM_GT -DCM_B -DCM_D
LD = ld
SRCS=	termcap.c tgoto.c tputs.c
OBJS=	termcap.o tgoto.o tputs.o
TAGSFILE=tags

.c.o:
	$(CC) $(CFLAGS) -c -p $*.c
	$(LD) $(LDFLAGS) -x -r -o profiled/$*.o $*.o
	$(CC) $(CFLAGS) -c $*.c
	$(LD) $(LDFLAGS) -X -r $*.o
	mv a.out $*.o

termcap.a termcap_p.a: ${OBJS}
	ar cr termcap.a ${OBJS}
	cd profiled; ar cr ../termcap_p.a ${OBJS}

install: termcap.a termcap_p.a
	install -c termcap.a ${DESTDIR}/usr/lib/libtermcap.a
	-rm -f ${DESTDIR}/usr/lib/libtermlib.a
	ln ${DESTDIR}/usr/lib/libtermcap.a ${DESTDIR}/usr/lib/libtermlib.a
	ranlib ${DESTDIR}/usr/lib/libtermcap.a
	install -c termcap_p.a ${DESTDIR}/usr/lib/libtermcap_p.a
	-rm -f ${DESTDIR}/usr/lib/libtermlib_p.a
	ln ${DESTDIR}/usr/lib/libtermcap_p.a ${DESTDIR}/usr/lib/libtermlib_p.a
	ranlib ${DESTDIR}/usr/lib/libtermcap_p.a

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	-rm -f *.o profiled/*.o
	-rm -f termcap.a termcap_p.a

VGRIND=	csh /usr/ucb/vgrind
vgrind:
	cp /dev/null index
	${VGRIND} -h "Termcap library" termcap.c tputs.c tgoto.c
	${VGRIND} -h "Termcap library" -x index
