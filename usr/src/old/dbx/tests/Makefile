#	@(#)Makefile	5.4 (Berkeley) 1/27/91

TESTDBX=	../../tdbx
#SUBDIR=	mod cc pc f77
SUBDIR=	cc

passed: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make TESTDBX=${TESTDBX}
	@echo ""
	@echo "passed tests"

clean: FRC
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend: FRC
install: FRC
lint: FRC
tags: FRC
FRC:
