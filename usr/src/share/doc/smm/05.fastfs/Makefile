#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
SRCS=	input.me
MACROS=	-me
PRINTER=Pdp




SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t
TROFF=	itroff

fs:	${SRCS}
	eqn ${SRCS} | tbl | ${TROFF} -ms -t > fs

paper:	fs
	lpr -Pimagen -t fs
