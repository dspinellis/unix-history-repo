#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) %G%
#
TOCS=	toc1 toc2 toc3 toc3f toc4 toc5 toc6 toc7 toc8 \
	tocx1 tocx2 tocx3 tocx3f tocx4 tocx5 tocx6 tocx7 tocx8
MACROS=	-msU
PRINTER=Pdp
TROFF=	ditroff -${PRINTER}

all: title.${PRINTER} preface.${PRINTER} intro.${PRINTER} \
     toc.${PRINTER} ptx.${PRINTER}
	lpr -${PRINTER} -n title.${PRINTER}
	lpr -${PRINTER} -n preface.${PRINTER}
	lpr -${PRINTER} -n intro.${PRINTER}
	lpr -${PRINTER} -n toc.${PRINTER}
	lpr -${PRINTER} -n ptx.${PRINTER}

title.${PRINTER}: title.urm
	${TROFF} -t title.urm >title.${PRINTER}

preface.${PRINTER}: preface.ms
	${TROFF} ${MACROS} -t preface.ms >preface.${PRINTER}

intro.${PRINTER}: intro.ms
	${TROFF} ${MACROS} -t intro.ms >intro.${PRINTER}

toc.${PRINTER}: toc.in ptxx
	${TROFF} -t toc.in >toc.${PRINTER}

ptx.${PRINTER}: ptx.in ptxx
	${TROFF} -t ptx.in >ptx.${PRINTER}

ptxx:
	tocrc all
	tocrc t

clean:
	rm -f title.[PT]* preface.[PT]* intro.[PT]* toc.[PT]* ptx.[PT]* \
	${TOCS} ptxx *.spell errs Errs make.out
