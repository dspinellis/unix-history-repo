#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) %G%
#
#
#  Makefile for -man macros
#

SRCS=	tmac.an tmac.an.new tmac.an6n tmac.an6t

all:	${SRCS}

install:${SRCS}
	for i in ${SRCS} do (install -c $$i ${DESTDIR}/usr/lib/tmac/$$i); done

clean:
