#	@(#)Makefile.inc	8.1 (Berkeley) %G%

CFLAGS+=-I/sys
SRCS+=	isinf.c
SRCS+=	_setjmp.s alloca.s fabs.s ldexp.s modf.s setjmp.s
SRCS+=	udiv.s urem.s
