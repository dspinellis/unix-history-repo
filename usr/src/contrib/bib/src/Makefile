#	@(#)Makefile	2.6	6/10/85
#
CFLAGS = -DINCORE
#CFLAGS = -DINCORE -g
CC = cc $(CFLAGS)
.c.o:
	$(CC) -c $*.c

INVo        = invert.o streams.o  makekey.o
INVc        = invert.c streams.c  makekey.c
LOOKo       = lookup.o locate.o alpha.seek.o streams.o makekey.o
LOOKc       = lookup.c locate.c alpha.seek.c streams.c makekey.c
NEWBIBo     = newbib.o newbibargs.o locate.o alpha.seek.o streams.o makekey.o
BIBo        = bib.o bibargs.o locate.o alpha.seek.o streams.o makekey.o
BIBc        = bib.c bibargs.c locate.c alpha.seek.c streams.c makekey.c
LISTo       = listrefs.o bibargs.o streams.o makekey.o
NEWLISTo    = listrefs.o newbibargs.o streams.o makekey.o
LISTc       = listrefs.c bibargs.c streams.c makekey.c

SRCS = \
	bib.c \
	bibargs.c \
	alpha.seek.c \
	bib.h \
	invert.c \
	listrefs.c \
	locate.c \
	lookup.c \
	makekey.c \
	streams.c \
	streams.h

all:	invert lookup listrefs bib

install: all
	install -s -c invert $(DESTDIR)/usr/new/invert
	install -s -c lookup $(DESTDIR)/usr/new/lookup
	install -s -c listrefs $(DESTDIR)/usr/new/listrefs
	install -s -c bib $(DESTDIR)/usr/new/bib

invert:  $(INVo)
	$(CC)   $(INVo)   -o invert

invert.lint:  $(INVc) bib.h streams.h
	lint  $(INVc) >invert.lint

lookup: $(LOOKo)
	$(CC)   $(LOOKo)  -o lookup

lookup.lint: $(LOOKc) bib.h streams.h
	lint $(LOOKc) >lookup.lint

bib: $(BIBo)
	$(CC)   $(BIBo)  -o bib

bib.lint: $(BIBc) bib.h streams.h
	lint $(BIBc) >bib.lint

listrefs: $(LISTo)
	$(CC)   $(LISTo)  -o listrefs

newlisttrefs: $(NEWLISTo)
	$(CC)   $(NEWLISTo)  -o newlistrefs

bib.o:		bib.h
bibargs.o:	bib.h
listrefs.o:	bib.h
invert.o:       streams.h bib.h

locate.o:       streams.h bib.h
makekey.o:	bib.h
alpha.seek.o:   streams.h
streams.o:      streams.h

$(SRCS) :
	sccs get $@

bibdoc:
	tbl doc/bibdoc.ms | vtroff -ms
	vtroff -man doc/invert.man
	vtroff -man doc/bib.man

doc/INDEX:	doc/testrefs
	cd doc; invert -p INDEX testrefs

styleman:
	sed -e 's/STYLE/stdn - standard numeric/' tstyles | bib -tstdn >show.stdn
	sed -e 's/STYLE/stdsn - standard sorted numeric/' tstyles | bib -tstdsn >show.stdsn
	sed -e 's/STYLE/stda - standard alphabetic/' tstyles | bib -tstda >show.stda
	sed -e 's/STYLE/opena - open alphabetic/' tstyles | bib -topena >show.opena
	sed -e 's/STYLE/openn - open numeric/' tstyles | bib -topenn >show.openn
	sed -e 's/STYLE/astro - astrophysical journal style/' tstyles | bib -tastro >show.astro
	sed -e 's/STYLE/hnf - Hanson Normal Form/' tstyles | bib -thnf >show.hnf
	sed -e 's/STYLE/list - listrefs default/' tstyles | bib -tlist >show.list
	sed -e 's/STYLE/spe - S P and E Journal style/' tstyles | bib -tspe2 >show.spe
	sed -e 's/STYLE/supn - superscripted numeric style/' tstyles | bib -tsupn >show.supn
	sed -e 's/STYLE/foot - footnoted citations/' tstyles | bib -tfoot >show.foot
	vtroff -ms styletop show.*

clean:	
	-rm -f *.o bib invert listrefs lookup tags
