#	from: @(#)Makefile.inc	5.3 (Berkeley) 2/20/91
#	$Id: Makefile.inc,v 1.4 1993/10/07 00:24:30 cgd Exp $

# yp sources
.PATH: ${.CURDIR}/arch/${MACHINE}/yp ${.CURDIR}/yp

SRCS+=	xdryp.c yplib.c

