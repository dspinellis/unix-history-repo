CFLAGS=	-O -DCM_N -DCM_GT -DCM_B -DCM_D
LD = ld

.c.o:
	$(CC) $(CFLAGS) -c -p $*.c
	$(LD) $(LDFLAGS) -x -r -o profiled/$*.o $*.o
	$(CC) $(CFLAGS) -c $*.c
	$(LD) $(LDFLAGS) -X -r $*.o
	mv a.out $*.o

termcap.a termcap_p.a: termcap.o tgoto.o tputs.o
	ar cr termcap.a termcap.o tgoto.o tputs.o
	cd profiled; ar cr ../termcap_p.a termcap.o tgoto.o tputs.o

clean:
	-rm -f *.o profiled/*.o
	-rm -f termcap.a termcap_p.a

install: termcap.a termcap_p.a
	install -c termcap.a ${DESTDIR}/usr/lib/libtermcap.a
	-rm -f ${DESTDIR}/usr/lib/libtermlib.a
	ln ${DESTDIR}/usr/lib/libtermcap.a ${DESTDIR}/usr/lib/libtermlib.a
	ranlib ${DESTDIR}/usr/lib/libtermcap.a
	install -c termcap_p.a ${DESTDIR}/usr/lib/libtermcap_p.a
	-rm -f ${DESTDIR}/usr/lib/libtermlib_p.a
	ln ${DESTDIR}/usr/lib/libtermcap_p.a ${DESTDIR}/usr/lib/libtermlib_p.a
	ranlib ${DESTDIR}/usr/lib/libtermcap_p.a

VGRIND=	csh /usr/ucb/vgrind
vgrind:
	cp /dev/null index
	${VGRIND} -h "Termcap library" termcap.c tputs.c tgoto.c
	${VGRIND} -h "Termcap library" -x index
