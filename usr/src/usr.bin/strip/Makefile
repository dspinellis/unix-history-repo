#	@(#)Makefile	5.4 (Berkeley) 5/11/90

PROG=	strip

install: maninstall
	install -c -o ${BINOWN} -g ${BINOWN} -m ${BINMODE} strip \
	    ${DESTDIR}${BINDIR}
	./strip ${DESTDIR}${BINDIR}/strip
	rm -f ./strip

.include <bsd.prog.mk>
