#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2 (Berkeley) 4/28/86
#
#	makefile for screen package documentation
#
NROFF=	nroff
TROFF=	ditroff
TBL=	dtbl -Pdp
VFONT=	/usr/lib/vfontedpr
NFLAGS=	-Tcrt
TFLAGS= -Pdp
CFLAGS=	-O -n -s
BINDIR=	/usr/doc/ps1/18.curses

DOCS=	doc.I doc.II doc.III doc.IV
RAW=	macros c_macros appen.B appen.A appen.C intro.0 intro.1 \
	intro.2 intro.3 intro.4 intro.5 ${DOCS} curses.3x

CRAW=	win_st.c twinkle1.c twinkle2.c life.c
CSOURCE=win_st.gr twinkle1.gr twinkle2.gr life.gr
SOURCE=	macros appen.A.tbl appen.B appen.C intro.0 intro.1 \
	intro.2.tbl intro.3 intro.4 intro.5

.SUFFIXES:
.SUFFIXES: .c .gr

#
# this section formats C input source into nice troffable (or nroffable)
# versions.  It uses the capabilites of "vgrind", which sets keywords in
# bold font, and comments in italics.
#

# Don't re-run vgrind unless you want to patch the output files.
.c.gr:
	${VFONT} $*.c | grep -v "^'wh" > $*.gr

troff: Master macros ${SOURCE}
	${TROFF} -me ${TFLAGS} Master ;

Curses.doc: ${VFONT} Master macros ${SOURCE}
	${NROFF} -me ${NFLAGS} Master > Curses.doc

${CSOURCE}: ${VFONT}

intro.5: ${DOCS}
appen.B: win_st.gr
appen.C: twinkle1.gr life.gr twinkle2.gr

intro.2.tbl: intro.2
	${TBL} intro.2 > intro.2.tbl

appen.A.tbl: appen.A
	${TBL} appen.A > appen.A.tbl

vfontedpr: vfontedpr.c
	${CC} ${CFLAGS} -o vfontedpr vfontedpr.c

spell: ${SOURCE}
	/bin/csh ./Spellit ${SOURCE} ${DOCS}

install:
	cp Makefile ${RAW} ${BINDIR}

ar:
	ar crv curdoc.ar Master ${RAW} ${CRAW} Makefile

lpr: Curses
	-lpr Curses ; lpq
	-lpq

clean:
	rm -f ${CSOURCE} appen.A.tbl intro.2.tbl curses.tar vfontedpr \
		Curses.doc errs Errs
