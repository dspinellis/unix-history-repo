#	@(#)Makefile	5.4 (Berkeley) %G%

.if	(${MACHINE} == "tahoe")
SUBDIR=	c2 cc ccom f1 lint
.elif	(${MACHINE} == "vax")
SUBDIR=	c2 cc ccom f1 lint sccom
.endif

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 pcc.h /usr/include

.include <bsd.subdir.mk>
