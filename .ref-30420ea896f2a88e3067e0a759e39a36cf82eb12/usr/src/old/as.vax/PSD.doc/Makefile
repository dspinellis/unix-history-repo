#
# Copyright (c) 1983 Regents of the University of California
#
#	@(#)Makefile	1.1	(Berkeley)	%G%
#
MAN = as.man.1
DOC = asdocs0.me asdocs1.me asdocs2.me asdocs3.me asdocs4.me
#
# Change these to change the way the document is printed out;
# the document is written using -me, which always must be present
#
FMT = vtroff -me
FMTOPTIONS = -t
FMTDST = docs.t.out

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
	cat $(DOCS) | $(FMT) $(FMTOPTIONS) > $(FMTDST)
	-rm *.TME

.me.TME:
	soelim $*.me | tbl > $*.tmp
	eqn /usr/pub/eqnchar $*.tmp > $*.TME
	rm $*.tmp
