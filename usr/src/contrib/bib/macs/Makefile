#
#	@(#)Makefile	1.2	(Berkeley)	84/01/21
#
DEST	      = $(DESTDIR)/usr/lib/bmac

MAKEFILE      = Makefile

PRINT	      = vtroff

SRCS	      = bib.astro \
		bib.foot \
		bib.hnf \
		bib.list \
		bib.opena \
		bib.openn \
		bib.spe \
		bib.spe2 \
		bib.stda \
		bib.stdafull \
		bib.stdn \
		bib.stdsn \
		bib.supn \
		bibinc.fullnames \
		bibinc.publishers \
		bibinc.shortnames \
		bibmac.me \
		bmac.open \
		bmac.std \
		common

all:;

clean:;

depend:;	@mkmf -f $(MAKEFILE)

install:;	@echo Installing $(SRCS) in $(DEST)
		-for i in $(SRCS); do rm -f $(DEST)/$$i; cp $$i $(DEST)/$$i; done

update:;
