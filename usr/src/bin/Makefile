#	@(#)Makefile	5.31 (Berkeley) %G%

SUBDIR=	cat chmod cp csh date dd df echo ed expr hostname kill ls mkdir \
	mv ps pwd rcp rm rmail rmdir sh sleep stty sync tar test

.if   make(clean) || make(cleandir)
SUBDIR+=
.elif ${MACHINE} == "tahoe"
SUBDIR+=adb
.elif ${MACHINE} == "vax"
SUBDIR+=adb
.endif

.include <bsd.subdir.mk>
