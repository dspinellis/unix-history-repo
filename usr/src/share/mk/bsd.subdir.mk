.include <bsd.global.mk>

.MAIN: all

STDALL STDDEPEND STDCLEAN STDCLEANDIR STDLINT STDINSTALL STDTAGS: .USE
	@for entry in ${SUBDIR}; do \
		(if test -d ${.CURDIR}/$${entry}.${MACHINE}; then \
			echo "===> $${entry}.${MACHINE}"; \
			cd ${.CURDIR}/$${entry}.${MACHINE}; \
		else \
			echo "===> $$entry"; \
			cd ${.CURDIR}/$${entry}; \
		fi; \
		${MAKE} ${.TARGET}) \
	done

${SUBDIR}::
	@if test -d ${.TARGET}.${MACHINE}; then \
		cd ${.CURDIR}/${.TARGET}.${MACHINE}; \
	else \
		cd ${.CURDIR}/${.TARGET}; \
	fi; \
	${MAKE} all

.if !target(all)
all: STDALL
.endif

.if !target(clean)
clean: STDCLEAN
.endif

.if !target(cleandir)
cleandir: STDCLEANDIR
.endif

.if !target(depend)
depend: STDDEPEND
.endif

.if !target(lint)
lint: STDLINT
.endif

.if !target(tags)
tags: STDTAGS
.endif

.if !target(install)
.if target(beforeinstall)
install: beforeinstall
install: STDINSTALL
.else
install: STDINSTALL
.endif
.endif
