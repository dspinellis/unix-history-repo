#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
#	makefile for screen package documentation
#
NROFF=	nroff
TROFF=	vtroff
VFONT=	/usr/lib/vfontedpr
NFLAGS=	-Tcrt
TFLAGS=
CFLAGS=	-O -n -s
BINDIR=	/usr/doc/curses
TARFILE=cf curdoc.tar

DOCS=	doc.I doc.II doc.III doc.IV
RAW=	macros c_macros appen.B appen.A appen.C disclaim intro.0 intro.1 \
	intro.2 intro.3 intro.4 intro.5 ${DOCS} curses.3

CRAW=	win_st.c twinkle1.c twinkle2.c life.c
CSOURCE=win_st.gr twinkle1.gr twinkle2.gr life.gr
SOURCE=	macros appen.A.tbl appen.B appen.C disclaim intro.0 intro.1 \
	intro.2.tbl intro.3 intro.4 intro.5

#.SUFFIXES:
#.SUFFIXES: .c .gr

#
# this section formats C input source into nice troffable (or nroffable)
# versions.  It uses the capabilites of "vgrind", which sets keywords in
# bold font, and comments in italics.
#

Curses.doc: ${VFONT} Master macros ${SOURCE}
	${NROFF} -me ${NFLAGS} Master > Curses.doc

# Don't re-run vgrind unless you want to patch the output files.
#.c.gr:
#	${VFONT} $*.c > $*.gr

${CSOURCE}: ${VFONT}
troff: Master macros ${SOURCE}
	${TROFF} -me ${TFLAGS} Master ;

intro.5: ${DOCS}
appen.B: win_st.gr
appen.C: twinkle1.gr life.gr twinkle2.gr

intro.2.tbl: intro.2
	tbl intro.2 > intro.2.tbl

appen.A.tbl: appen.A
	tbl appen.A > appen.A.tbl

vfontedpr: vfontedpr.c
	${CC} ${CFLAGS} -o vfontedpr vfontedpr.c

spell: ${SOURCE}
	/bin/csh ./Spellit ${SOURCE} ${DOCS}

install:
	cp Makefile ${RAW} ${BINDIR}

tar:
	tar rv${TARFILE} Master ${RAW} ${CRAW} Makefile vfontedpr.c

lpr: Curses
	-lpr Curses ; lpq
	-lpq

clean:
	rm -f ${CSOURCE} appen.A.tbl intro.2.tbl curses.tar vfontedpr
