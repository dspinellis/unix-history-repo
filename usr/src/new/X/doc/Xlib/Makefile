# Copyright 1985, Massachusetts Institute of Technology
# Makefile for the Xlib document.

TROFF= ditroff -Tpsc
PRINTER= lpr -n -Pln03-bldge40-4
FILES= macros.t\
	abstract.t\
	ch01a.t\
	ch02a.t\
	ch03a.t ch03b.t\
	ch04a.t\
	ch05a.t ch05b.t ch05c.t\
	ch06a.t ch06b.t ch06c.t ch06d.t ch06e.t\
	ch07a.t\
	ch08a.t\
	ch09a.t\
	ch10a.t\
	ch11a.t ch11b.t ch11c.t\
	ch12a.t ch12b.t ch12c.t ch12d.t ch12e.t\
	ch13a.t\
	credits.t\
	appendix

all: paper

paper: $(FILES)
	eqn ${FILES} | tbl |${TROFF} -ms 2> index.raw | ${PRINTER}

preview:$(FILES)
	neqn ${FILES} | tbl | nroff -ms 2> index.nraw | colcrt

index.raw:
	eqn ${FILES} | tbl | ${TROFF} -ms -t > /dev/null 2> index.raw|\
	${PRINTER}

index:	index.raw
	sort +1 +0n -1n index.raw | awk -f fixindex.awk\
		| awk -f block.awk > index.troff
	cat indexmacros.t index.troff | ${TROFF} -me | ${PRINTER}
	rm -f index.troff

clean:
	rm -f *~ \#* *.bak errs index.traw index.nraw
