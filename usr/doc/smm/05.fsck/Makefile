#
# Copyright (c) 1982 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.2 (Berkeley) 7/18/85
#
SRCS=	0.t 1.t 2.t 3.t 4.t
TROFF=	ditroff
PRINTER=-Pdp
TYPE=	-n

paper:	fsck.t
	lpr ${PRINTER} ${TYPE} fsck.t

fsck.t:	${SRCS}
	${TROFF} ${PRINTER} -ms -t ${SRCS} > fsck.t

clean:
	rm -f fsck.t
