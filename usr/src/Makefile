#	@(#)Makefile	5.8 (Berkeley) %G%

SUBDIR=	bin contrib games include kerberosIV lib libexec sbin share \
	usr.bin usr.sbin

afterinstall:
	(cd share/man && make makedb)

build:
	(cd include && make install)
	make cleandir
	(cd lib && make depend && make && make install)
	(cd kerberosIV && make && make install)
	make depend && make && make install
	
.include <bsd.subdir.mk>
