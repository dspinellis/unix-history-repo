#
# Copyright (c) 1982 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.1 (Berkeley) %G%
#
SRCS=	0.t 1.t 2.t 3.t 4.t
TROFF=	itroff
TYPE=	-t

paper:	fsck.t
	lpr -Pimagen ${TYPE} fsck.t

fsck.t:	${SRCS}
	${TROFF} -ms -t ${SRCS} > fsck.t

clean:
	rm -f fsck.t
