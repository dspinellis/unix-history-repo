#	@(#)Makefile	4.23 (Berkeley) %G%

SUBDIR=	bin contrib games include kerberosIV lib libexec old pgrm sbin \
	share usr.bin usr.sbin

build:
	${MAKE} cleandir

	@echo "+++ includes"
	cd include; ${MAKE} depend all install clean

.if ${MACHINE} == "vax" || ${MACHINE} == "tahoe"
	@echo "+++ C preprocessor, compiler, loader"
	cd pgrm/cpp; ${MAKE} depend all install clean
	cd libexec/pcc; ${MAKE} depend all install clean
	cd pgrm/ld; ${MAKE} depend all install clean

	@echo "+++ C library"
	cd lib/libc; ${MAKE} depend all install clean

	@echo "+++ C preprocessor, compiler, loader (second time)"
	cd pgrm/cpp; ${MAKE} all install
	cd libexec/pcc; ${MAKE} all install
	cd pgrm/ld; ${MAKE} all install
.endif

	@echo "+++ libraries"
	cd lib; ${MAKE} depend all install all
	cd kerberosIV; ${MAKE} depend all install all

	@echo "+++ C library tags"
	cd lib/libc; rm -f tags; ${MAKE} tags; \
	    install -c -o ${BINOWN} -g ${BINGRP} -m 444 tags /usr/libdata/tags

	${MAKE} depend all

.include <bsd.subdir.mk>
