#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.3 (Berkeley) 5/28/90
#
SRCS=	bibdoc.ms
MACROS=	-i char.macros -ms
PRINTER=Pps
BIB=	bib
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER} INDEX
	lpr -${PRINTER} -n paper.${PRINTER}

paper.${PRINTER}: ${SRCS} INDEX
	${BIB} ${SRCS} | tbl | ${TROFF} ${MACROS} -t > paper.${PRINTER}

clean:
	rm -f paper.[PT]* *.spell errs Errs make.out

spell:	bibdoc.ms
	spell bibdoc.ms | sort | comm -23 - spell.ok > bibdoc.spell 

INDEX:	testrefs
	invert testrefs
