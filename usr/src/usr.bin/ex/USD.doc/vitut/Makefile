#
# Copyright (c) 1986 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	6.2 (Berkeley) %G%
#
SRCS=	vi.in vi.chars
MACROS=	-msU
PRINTER=Pdp
TBL=	dtbl -${PRINTER}
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER} summary.${PRINTER} viapwh.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}
	lpr -${PRINTER} -n summary.${PRINTER}
	lpr -${PRINTER} -n viapwh.${PRINTER}

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${TROFF} ${MACROS} -t > paper.${PRINTER}

summary.${PRINTER}: vi.summary
	${TBL} vi.summary | ${TROFF} ${MACROS} -t > summary.${PRINTER}

viapwh.${PRINTER}: vi.apwh.ms
	${TROFF} ${MACROS} -t vi.viapwh > viapwh.${PRINTER}

clean:
	rm -f paper.[PT]* summary.[PT]* viapwh.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
