#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	f77
CFLAGS+=-DHERE=TAHOE -DTARGET=TAHOE -DFAMILY=PCC -DUCBVAXASM -DUCBPASS2 \
	-I${.CURDIR}/../include.tahoe
BINDIR=	/usr/bin

.include <bsd.prog.mk>
