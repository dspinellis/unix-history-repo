#	@(#)Makefile	5.4 (Berkeley) %G%

PROG=	strip

install: maninstall
	install -c -o ${BINOWN} -g ${BINOWN} -m ${BINMODE} strip \
	    ${DESTDIR}${BINDIR}
	./strip ${DESTDIR}${BINDIR}/strip
	rm -f ./strip

.include <bsd.prog.mk>
