#
# Copyright (c) 1986, 1993 Regents of the University of California.
# All rights reserved.
#
#	@(#)Makefile	6.3 (Berkeley) %G%
#
SRCS=	man1 man2 man3 man4 man5 man6 man7 man8

MACROS=	-ms
PRINTER=Pps
TBL=	tbl
TROFF=	groff

all: title.urm.${PRINTER} preface.${PRINTER} intro.${PRINTER} \
     toc.${PRINTER} ptx.${PRINTER} title.prm.${PRINTER}

print: title.urm.${PRINTER} preface.${PRINTER} intro.${PRINTER} \
     toc.${PRINTER} ptx.${PRINTER} title.prm.${PRINTER}
	lpr -${PRINTER} title.urm.${PRINTER}
	lpr -${PRINTER} preface.${PRINTER}
	lpr -${PRINTER} intro.${PRINTER}
	lpr -${PRINTER} toc.${PRINTER}
	lpr -${PRINTER} ptx.${PRINTER}
	lpr -${PRINTER} title.prm.${PRINTER}

title.urm.${PRINTER}: title.urm
	${TROFF} title.urm >title.urm.${PRINTER}

preface.${PRINTER}: preface.ms
	${TBL} preface.ms | ${TROFF} ${MACROS} >preface.${PRINTER}

intro.${PRINTER}: intro.ms
	${TROFF} ${MACROS} intro.ms >intro.${PRINTER}

toc.${PRINTER}: toc.in ptxx
	${TROFF} toc.in >toc.${PRINTER}

ptx.${PRINTER}: ptx.in ptxx
	${TROFF} ptx.in >ptx.${PRINTER}

title.prm.${PRINTER}: title.prm ptxx
	${TROFF} title.prm >title.prm.${PRINTER}

ptxx:
	tocrc

clean:
	rm -f title.urm.[PT]* preface.[PT]* intro.[PT]* toc.[PT]* ptx.[PT]* \
	title.prm.[PT]* toc? tocx? ptxx *.spell errs Errs make.out
