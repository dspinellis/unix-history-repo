#	@(#)bsd.subdir.mk	5.9 (Berkeley) 2/1/91

.MAIN: all

STRIP?=	-s

BINGRP?=	bin
BINOWN?=	bin
BINMODE?=	555

_SUBDIRUSE: .USE
	@for entry in ${SUBDIR}; do \
		(if test -d ${.CURDIR}/$${entry}.${MACHINE}; then \
			echo "===> $${entry}.${MACHINE}"; \
			cd ${.CURDIR}/$${entry}.${MACHINE}; \
		else \
			echo "===> $$entry"; \
			cd ${.CURDIR}/$${entry}; \
		fi; \
		${MAKE} ${.TARGET:realinstall=install}); \
	done

${SUBDIR}::
	@if test -d ${.TARGET}.${MACHINE}; then \
		cd ${.CURDIR}/${.TARGET}.${MACHINE}; \
	else \
		cd ${.CURDIR}/${.TARGET}; \
	fi; \
	${MAKE} all

.if !target(all)
all: _SUBDIRUSE
.endif

.if !target(clean)
clean: _SUBDIRUSE
.endif

.if !target(cleandir)
cleandir: _SUBDIRUSE
.endif

.if !target(depend)
depend: _SUBDIRUSE
.endif

.if !target(install)
.if !target(beforeinstall)
beforeinstall:
.endif
.if !target(afterinstall)
afterinstall:
.endif
install: afterinstall
afterinstall: realinstall
realinstall: beforeinstall _SUBDIRUSE
.endif

.if !target(lint)
lint: _SUBDIRUSE
.endif

.if !target(obj)
obj: _SUBDIRUSE
.endif

.if !target(tags)
tags: _SUBDIRUSE
.endif
