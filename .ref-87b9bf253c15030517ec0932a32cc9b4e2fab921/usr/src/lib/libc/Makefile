#	@(#)Makefile	4.4	{Berkeley}	82/12/17
#
#	The system has two monitoring schemes, utilizing either ``prof''
#	or ``gprof''. DFLMON determines the default profiler; mon.o
#	specifies that ``prof'' is the default, gmon.o specifies that
#	``gprof'' is the default.
#
DESTDIR=
INSTALL=install -m 644
DFLMON=	mon.o
ALL=	csu sys gen stdio net

libc.a libc_p.a: ${ALL}
	cd csu; make
	cd sys; make
	cd gen; make
	cd stdio; make
	cd net; make
	-mkdir tmp
	cd tmp; ar x ../sys/syslib
	cd tmp; ar x ../gen/genlib
	cd tmp; ar x ../stdio/stdiolib
	cd tmp; ar x ../net/netlib
	ls tmp/*.o | sort -t/ +1 > t1
	ar cr libc.a `lorder \`cat t1\` | tsort` csu/${DFLMON}
	ar ma flsbuf.o libc.a exit.o
	rm -f t1 tmp/*
	cd tmp; ar x ../sys/syslib_p
	cd tmp; ar x ../gen/genlib_p
	cd tmp; ar x ../stdio/stdiolib_p
	cd tmp; ar x ../net/netlib_p
	ls tmp/*.o | sort -t/ +1 > t1
	ar cr libc_p.a `lorder \`cat t1\` | tsort` csu/${DFLMON}
	ar ma flsbuf.o libc_p.a exit.o
	rm -rf t1 tmp

clean:
	for i in ${ALL}; do cd $$i; make clean; cd ..; done
	rm -f t1 *.o

errlst.o: gen/errlst.c
	cc -S gen/errlst.c
	ed - <gen/:errfix errlst.s
	as -o errlst.o errlst.s
	rm errlst.s

install: libc.a libc_p.a
	${INSTALL} libc.a ${DESTDIR}/lib/libc.a
	ranlib ${DESTDIR}/lib/libc.a
	${INSTALL} libc_p.a ${DESTDIR}/usr/lib/libc_p.a
	ranlib ${DESTDIR}/usr/lib/libc_p.a
	cd csu; make DESTDIR=${DESTDIR} install
