#
# Copyright (c) 1989 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.3 (Berkeley) %G%
#
SRCS=	0.t 1.t 2.t 3.t
MACROS=	-ms
PRINTER=Plz
TROFF=	ditroff -${PRINTER}
REFER=	/a/staff/mckusick/book/ref/refer -m -n -e -l -s -p ref.bib

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${TROFF} ${MACROS} -t tmac.srefs - | \
		psdit >paper.${PRINTER}

print: paper.${PRINTER}
	lpr -${PRINTER} paper.${PRINTER}

clean:
	rm -f *.spell errs Errs make.out paper.${PRINTER}

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
