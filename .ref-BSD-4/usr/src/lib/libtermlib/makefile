CFLAGS=	-O -DCM_N -DCM_GT -DCM_B -DCM_D

termcap.a: termcap.o tgoto.o tputs.o
	ar cr termcap.a termcap.o tgoto.o tputs.o

clean:
	-rm -f *.o
	-rm -f termcap.a

install: termcap.a
	install -c termcap.a ${DESTDIR}/usr/lib/libtermcap.a
	-rm -f ${DESTDIR}/usr/lib/libtermlib.a
	ln ${DESTDIR}/usr/lib/libtermcap.a ${DESTDIR}/usr/lib/libtermlib.a
	ranlib ${DESTDIR}/usr/lib/libtermcap.a

VGRIND=	csh /usr/ucb/vgrind
vgrind:
	cp /dev/null index
	${VGRIND} -h "Termcap library" termcap.c tputs.c tgoto.c
	${VGRIND} -h "Termcap library" -x index
