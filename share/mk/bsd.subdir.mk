#	@(#)bsd.subdir.mk	5.9 (Berkeley) 2/1/91

.MAIN: all

STRIP?=	-s

BINGRP?=	bin
BINOWN?=	bin
BINMODE?=	555

.END: .MAKE
	@for entry in ${SUBDIR}; do \
		(if test -d ${.CURDIR}/$${entry}.${MACHINE}; then \
			echo "===> ${DIRPRFX}$${entry}.${MACHINE}"; \
			edir=$${entry}.${MACHINE}; \
			cd ${.CURDIR}/$${edir}; \
		else \
			echo "===> ${DIRPRFX}$$entry"; \
			edir=$${entry}; \
			cd ${.CURDIR}/$${edir}; \
		fi; \
		${MAKE} ${.TARGETS} DIRPRFX=${DIRPRFX}$$edir/); \
	done

${SUBDIR}::
	@if test -d ${.TARGET}.${MACHINE}; then \
		cd ${.CURDIR}/${.TARGET}.${MACHINE}; \
	else \
		cd ${.CURDIR}/${.TARGET}; \
	fi; \
	${MAKE} all

.if !target(all)
all:
.endif

.if !target(clean)
clean:
.endif

.if !target(cleandir)
cleandir:
.endif

.if !target(depend)
depend:
.endif

.if !target (maninstall)
maninstall:
.endif

.if !target(install)
install:
.endif

.if !target(beforeinstall)
beforeinstall:
.endif

.if !target(afterinstall)
afterinstall:
.endif

.if !target(lint)
lint:
.endif

.if !target(obj)
obj:
.endif

.if !target(tags)
tags:
.endif
