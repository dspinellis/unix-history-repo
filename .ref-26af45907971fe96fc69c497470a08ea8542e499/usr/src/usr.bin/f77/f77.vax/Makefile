#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	f77
CFLAGS+=-DHERE=VAX -DTARGET=VAX -DFAMILY=PCC -DUCBVAXASM -DUCBPASS2 \
	-I${.CURDIR}/../include.vax
BINDIR=	/usr/bin

.include <bsd.prog.mk>
