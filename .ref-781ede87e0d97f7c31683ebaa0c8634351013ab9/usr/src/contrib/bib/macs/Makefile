#
#	@(#)Makefile	1.7	87/11/16
#
DEST	      = $(DESTDIR)/usr/new/lib/bmac

MAKEFILE      = Makefile

PRINT	      = vtroff

SRCS	      = bib.astro \
		bib.compsurv \
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

install:	$(DEST)
		@echo Installing $(SRCS) in $(DEST)
		-for i in $(SRCS); do rm -f $(DEST)/$$i; cp $$i $(DEST)/$$i; done

$(DEST):
		mkdir $(DEST)

update:;
