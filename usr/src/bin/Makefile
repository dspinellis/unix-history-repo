#	@(#)Makefile	5.35 (Berkeley) %G%

SUBDIR=	cat chmod csh date dd df echo ed expr hostname kill ln ls \
	mkdir mv ps pwd rcp rm rmail rmdir sh sleep stty sync tar test

.if	${MACHINE} == "hp300"
SUBDIR+=cp
.elif	${MACHINE} == "i386"
SUBDIR+=cp
.elif	${MACHINE} == "mips"
SUBDIR+=cp
.elif	${MACHINE} == "sparc"
SUBDIR+=cp
.elif	${MACHINE} == "tahoe"
SUBDIR+=adb
.elif	${MACHINE} == "vax"
SUBDIR+=adb
.endif

.include <bsd.subdir.mk>
