#	@(#)Makefile	5.32 (Berkeley) %G%

SUBDIR=	cat chmod cp csh date dd df echo ed expr hostname kill ls mkdir \
	mv ps pwd rcp rm rmail rmdir sh sleep stty sync tar test

.if   ${MACHINE} == "tahoe"
SUBDIR+=adb
.elif ${MACHINE} == "vax"
SUBDIR+=adb
.endif

.include <bsd.subdir.mk>
