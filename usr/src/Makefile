#	@(#)Makefile	5.5 (Berkeley) %G%

SUBDIR=	bin contrib games include kerberosIV lib libexec sbin share \
	usr.bin usr.sbin

afterinstall:
	(cd share/man && make makedb)

.include <bsd.subdir.mk>
