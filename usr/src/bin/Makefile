#	@(#)Makefile	5.34 (Berkeley) %G%

SUBDIR=	cat chmod csh date dd df echo ed expr hostname kill ln ls \
	mkdir mv ps pwd rcp rm rmail rmdir sh sleep stty sync tar test

.if   ${MACHINE} == "tahoe"
SUBDIR+=adb cp
.elif ${MACHINE} == "vax"
SUBDIR+=adb cp
.endif

.include <bsd.subdir.mk>
