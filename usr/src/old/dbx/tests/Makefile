#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#

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
