# A Makefile for handling subdirectories.
# Machine dependent subdirectories take precedence.
#
#	@(#)bsd.subdir.mk	5.1 (Berkeley) %G%
#

# user defines:
#	SUBDIR -- the list of subdirectories to be processed

# the default target.
.MAIN: all

# The standard targets change to the subdirectory and make the
# target.
STDALL STDDEPEND STDCLEAN STDCLEANDIR STDLINT STDINSTALL STDTAGS: .USE
	@for entry in ${SUBDIR}; do \
		(echo "===> $$entry"; \
		if test -d $${entry}.${MACHINE}; then \
			cd $${entry}.${MACHINE}; \
		else \
			cd $${entry}; \
		fi; \
		${MAKE} ${.TARGET}) \
	done

# If the user has not specified the target, use the standard version.
all: STDALL
depend: STDDEPEND
clean: STDCLEAN
cleandir: STDCLEANDIR
lint: STDLINT
install: STDINSTALL
tags: STDTAGS

# If trying to make one of the subdirectories, change to it and make
# the default target.
${SUBDIR}:
	@if test -d ${.TARGET}.${MACHINE}; then \
		cd ${.TARGET}.${MACHINE}; \
	else \
		cd ${.TARGET}; \
	fi; \
	${MAKE}
