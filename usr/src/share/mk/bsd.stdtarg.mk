.if !target(clean)
clean: STDCLEAN
.endif

.if !target(cleandir)
cleandir: STDCLEANDIR
.endif

.if !target(depend)
depend: .depend
.depend: STDDEPEND
.endif

.if !target(lint)
lint: ${SRCS} STDLINT
.endif

.if !target(tags)
tags: ${SRCS} STDTAGS
.endif

.if !target(install)
.if target(beforeinstall)
install: beforeinstall
install: STDINSTALL mainstall
.else
install: STDINSTALL mainstall
.endif
.endif
