#	@(#)Makefile	2.1	%G%
#
CC = cc
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
	install -s -c invert $(DESTDIR)/usr/local/invert
	install -s -c lookup $(DESTDIR)/usr/local/lookup
	install -s -c listrefs $(DESTDIR)/usr/local/listrefs
	install -s -c bib $(DESTDIR)/usr/local/bib

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

clean:	
	-rm *.o bib invert listrefs lookup tags
