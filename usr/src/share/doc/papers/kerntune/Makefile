#
# Copyright (c) 1984 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	1.4 (Berkeley) %G%
#

DIR=	papers/kerntune
SRCS=	0.t 1.t 2.t 3.t 4.t
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${SOELIM} ${SRCS} | ${PIC} | ${TBL} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
