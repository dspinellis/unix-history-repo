#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
TROFF=	itroff
FILES=	abs.ms motivation.ms equip.ms methodology.ms tests.ms results.ms \
	conclusions.ms appendix.ms

paper:	${FILES}
	@tbl ${FILES} | ${TROFF} -ms

preview:${FILES}
	tbl ${FILES} | nroff -ms | colcrt
