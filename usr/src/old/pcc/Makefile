#	@(#)Makefile	5.3 (Berkeley) %G%

.if	(${MACHINE} == "tahoe")
SUBDIR=	c2 cc ccom f1
.elif	(${MACHINE} == "vax")
SUBDIR=	c2 cc ccom f1 sccom
.endif

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 pcc.h /usr/include

.include <bsd.subdir.mk>
