#
# Copyright (c) 1983 Regents of the University of California
#
#	@(#)Makefile	1.2	(Berkeley)	4/15/86
#
MAN = as.man.1
DOC = asdocs0.me asdocs1.me asdocs2.me asdocs3.me asdocs4.me
#
# Change these to change the way the document is printed out;
# the document is written using -me, which always must be present
#
FMT = ditroff
PKG = -me
PRINTER = -Pdp
# FMTOPTIONS = -t
FMTOPTIONS =
FMTDST = docs.t.out
EQN = deqn
TBL = dtbl
all: sources fmt

clean:
	-rm *.TME $(FMTDST)

srcs: sources
sources:  $(DOC) $(MAN)
$(DOC) $(MAN):
	sccs get $@
#
# Constructing the assembler documentation:
#	assembler reference manual
#	assembler manual page
#
.SUFFIXES: .me .TME .tbl

DOCS = asdocs0.me asdocs1.TME asdocs2.TME asdocs3.TME asdocs4.TME

docs.raw: $(DOCS)

fmt: $(DOCS)
	cat $(DOCS) | $(FMT) $(FMTOPTIONS) $(PKG) $(PRINTER)
	-rm *.TME

.me.TME:
	soelim $*.me | $(TBL) $(PRINTER) > $*.tmp
	$(EQN) $(PRINTER) /usr/pub/eqnchar $*.tmp > $*.TME
	rm $*.tmp
