#	@(#)Makefile	2.7	%G%
#
# 
CFLAGS = -DBASEDIR=\"$(DEST_BASE)\" -DINCORE -g
CC = cc $(CFLAGS)
.c.o:
	$(CC) -c $*.c

INVo        = invert.o streams.o  makekey.o
INVc        = invert.c streams.c  makekey.c
LOOKo       = lookup.o locate.o alpha.seek.o streams.o makekey.o
LOOKc       = lookup.c locate.c alpha.seek.c streams.c makekey.c
BIBo        = bib.o bibargs.o locate.o alpha.seek.o streams.o makekey.o
BIBc        = bib.c bibargs.c locate.c alpha.seek.c streams.c makekey.c
LISTo       = listrefs.o bibargs.o streams.o makekey.o
LISTc       = listrefs.c bibargs.c streams.c makekey.c
BIBINCo	    = bibinc.o
BIBINCc     = bibinc.c

SRCS = \
	adsdebug.h \
	bib.c \
	bibargs.c \
	bibinc.c \
	comments.h \
	alpha.seek.c \
	bib.h \
	invert.c \
	listrefs.c \
	locate.c \
	lookup.c \
	makekey.c \
	streams.c \
	streams.h

all:	invert lookup listrefs bib bibinc

install: all
	install -s -c bib $(DEST_BIN)/bib
	install -s -c invert $(DEST_BIN)/invert
	install -s -c lookup $(DEST_BIN)/lookup
	install -s -c listrefs $(DEST_BIN)/listrefs
	install -s -c bibinc $(DEST_BIN)/bibinc
	install -c bib2tib $(DEST_BIN)/bib2tib

invert:  $(INVo)
	$(CC)   $(INVo)   -o invert

invert.lint:  $(INVc) bib.h streams.h
	lint  $(INVc) >invert.lint

lookup: $(LOOKo)
	$(CC)   $(LOOKo)  -o lookup

lookup.lint: $(LOOKc) bib.h streams.h
	lint $(LOOKc) >lookup.lint

bibinc: $(BIBINCc) adsdebug.h comments.h streams.h
	$(CC)   $(BIBINCc)  -o bibinc

bib: $(BIBo)
	$(CC)   $(BIBo)  -o bib

bib.lint: $(BIBc) bib.h streams.h
	lint $(BIBc) >bib.lint

listrefs: $(LISTo)
	$(CC)   $(LISTo)  -o listrefs

bib.o:		bib.h
bibargs.o:	bib.h
listrefs.o:	bib.h
invert.o:       streams.h bib.h

locate.o:       streams.h bib.h
makekey.o:	bib.h
alpha.seek.o:   streams.h
streams.o:      streams.h bib.h

$(SRCS) :
	sccs get $@

clean:	
	-rm -f *.o bib invert listrefs lookup tags bibinc
