# $Header: /b/source/CVS/src/usr.bin/tn3270/tools/mkastosc/Makefile,v 1.3 1993/04/27 06:47:44 cgd Exp $

CFLAGS += -I${.CURDIR}/../mkhits -I${.CURDIR}/.. -I.

NOMAN = I mean it!

SRCS = mkastosc.c dohits.c asc_ebc.c ebc_disp.c
PROG = mkastosc

.include <bsd.prog.mk>

.PATH: ${.CURDIR}/../mkhits ${.CURDIR}/../../api
