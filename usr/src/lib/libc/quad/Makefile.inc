#	@(#)Makefile.inc	5.1 (Berkeley) %G%

# GCC quad support
SRCS+=	adddi3.c anddi3.c ashldi3.c ashrdi3.c bdiv.c cmpdi2.c divdi3.c \
	fixdfdi.c fixunsdfdi.c floatdidf.c iordi3.c lshldi3.c lshrdi3.c \
	moddi3.c muldi3.c negdi2.c one_cmpldi2.c subdi3.c ucmpdi2.c \
	udivdi3.c umoddi3.c xordi3.c

.PATH: ${.CURDIR}/${MACHINE}/quad ${.CURDIR}/quad
