#
# Copyright (c) 1986, 1993 Regents of the University of California.
# All rights reserved.
#
#	@(#)Makefile	8.1 (Berkeley) 7/5/93
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

toc.${PRINTER}: ${SRCS} toc.in ptxx
	${TROFF} toc.in >toc.${PRINTER}

ptx.${PRINTER}: ${SRCS} ptx.in ptxx
	${TROFF} ptx.in >ptx.${PRINTER}

title.prm.${PRINTER}: ${SRCS} title.prm ptxx
	${TROFF} title.prm >title.prm.${PRINTER}

ptxx:
	./tocrc

man1pages: man1
	rm -f man1.out
	for i in `cat man1`; do \
		sed -f groff.sed $$i | groff -man >>man1pages; \
	done;

man2pages: man2
	rm -f man2.out
	for i in `cat man2`; do groff -man $$i >>man2pages; done;

man3pages: man3
	rm -f man3.out
	for i in `cat man3`; do groff -man $$i >>man3pages; done;

man4pages: man4
	rm -f man4.out
	for i in `cat man4`; do groff -man $$i >>man4pages; done;

man5pages: man5
	rm -f man5.out
	for i in `cat man5`; do \
		sed -f groff.sed $$i | groff -man >>man5pages; \
	done;

man6pages: man6
	rm -f man6.out
	for i in `cat man6`; do groff -man $$i >>man6pages; done;

man7pages: man7
	rm -f man7.out
	for i in `cat man7`; do \
		sed -f groff.sed $$i | groff -man >>man7pages; \
	done;

man8pages: man8
	rm -f man8.out
	for i in `cat man8`; do groff -man $$i >>man8pages; done;

clean:
	rm -f title.urm.[PT]* preface.[PT]* intro.[PT]* toc.[PT]* ptx.[PT]* \
	title.prm.[PT]* toc? tocx? ptxx *.spell errs Errs make.out
