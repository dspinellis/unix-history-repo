#	@(#)Makefile	4.20 (Berkeley) %G%

# skip old
SUBDIR=	bin games include lib libexec old pgrm sbin share usr.bin usr.sbin

build:
	${MAKE} cleandir

	@echo +++ installing includes
	cd include; ${MAKE} depend all install clean

	@echo +++ installing C compiler
	cd pgrm/cpp; ${MAKE} depend all install clean
	cd libexec/c2.${MACHINE}; ${MAKE} depend all install clean
	cd libexec/pcc; ${MAKE} depend all install clean

	@echo +++ installing C library
	cd lib/libc; ${MAKE} depend all install clean

	@echo +++ re-installing C compiler
	cd pgrm/cpp; ${MAKE} all install all
	cd libexec/c2.${MACHINE}; ${MAKE} all install all
	cd libexec/pcc; ${MAKE} all install all

	@echo +++ installing all libraries
	cd lib; ${MAKE} depend all install all
	cd lib/libc; rm -f tags; ${MAKE} tags; \
	    install -c -o ${BINOWN} -g ${BINGRP} -m 444 tags /usr/libdata/tags

	@echo +++ installing C library tags file

	@echo +++ libraries done
	${MAKE}

OBJ=	/usr/obj
objlinks:
	-for file in `find ${SUBDIR:S/^/-f /g} name SCCS prune or type dir print`; do \
		if [ -s $$file/Makefile -a ! -d ${OBJ}/$$file ] ; then \
			rm -rf ${OBJ}/$$file; \
			mkdir -p ${OBJ}/$$file > /dev/null 2>&1 ; \
			rm -f $$file/obj; \
			ln -s ${OBJ}/$$file $$file/obj; \
		fi; \
	done

shadow:
	-for file in `find ${SUBDIR:S/^/-f /g} name SCCS prune or type dir print`; do \
		if [ -s $$file/obj ] ; then \
			rm -rf ${OBJ}/$$file; \
			mkdir -p ${OBJ}/$$file > /dev/null 2>&1 ; \
		fi; \
	done

.include <bsd.own.mk>
.include <bsd.subdir.mk>
