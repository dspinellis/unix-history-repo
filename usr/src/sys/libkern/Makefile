#	@(#)Makefile	7.2 (Berkeley) %G%

LIB=	kern
CFLAGS+=-I${.CURDIR} -g
SRCS=	adddi3.c anddi3.c ashldi3.c ashrdi3.c bcmp.c cmpdi2.c divdi3.c \
	ffs.c imax.c imin.c iordi3.c lmax.c lmin.c locc.c lshldi3.c \
	lshrdi3.c max.c min.c moddi3.c muldi3.c negdi2.c notdi2.c \
	qdivrem.c scanc.c skpc.c strcat.c strcpy.c strlen.c strncpy.c \
	subdi3.c ucmpdi2.c udivdi3.c ulmax.c ulmin.c umoddi3.c xordi3.c
NOPROFILE=noprofile

.include <bsd.lib.mk>
