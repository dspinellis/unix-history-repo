#	@(#)Makefile	5.6 (Berkeley) %G%

SUBDIR=	fortune

.ifmake (clean) || (cleandir)
SUBDIR+=datfiles
.endif

.ifmake !(install)
SUBDIR+=strfile
.else
SUBDIR+=datfiles
.endif

.include <bsd.subdir.mk>
