#
# Copyright (c) 1989 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	1.4 (Berkeley) %G%
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

spell: ${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
