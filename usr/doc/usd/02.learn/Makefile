#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 5/23/86
#
DOCS = p0 p1 p2 p3 p4 p5 p6 p7
#
#
# Change these to change the way the document is printed out
#
FMT = ditroff
PKG = -msU
PRINTER = -Pdp
FMTOPTIONS =
TBL = dtbl

all: sources fmt

clean:
	-rm $(FMTDST)

srcs: sources
sources:  $(DOCS)
$(DOCS):
	sccs get $@



docs.raw: $(DOCS)

fmt: $(DOCS)
	refer -p Refs -e $(DOCS) | $(TBL) $(PRINTER) | $(FMT) $(FMTOPTIONS) $(PKG) $(PRINTER)

aps: $(DOCS)
	refer -p Refs -e $(DOCS) | $(TBL) $(PRINTER) | $(FMT) $(FMTOPTIONS) $(PKG) $(PRINTER) | $(LPR)

indexable: $(DOCS)
	refer -p Refs -e $(DOCS) | $(TBL) $(PRINTER) | sed -f ../pre.sed | $(FMT) $(FMTOPTIONS) $(PKG) $(PRINTER) | sed -f ../post.sed >../learn.a

