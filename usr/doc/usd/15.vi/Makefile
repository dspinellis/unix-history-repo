#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 5/30/86
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
