DEST	      = $(DESTDIR)/usr/man/mann

MAKEFILE      = Makefile

PRINT	      = vtroff

SRCS	      = bib.1 \
		invert.1

SUFFIX	      = .1:s

all:;

clean:;

depend:;	@mkmf -f $(MAKEFILE)

install:
	${DESTDIR}/usr/man/manroff bib.1 > ${DESTDIR}/usr/new/man/cat1/bib.0
	${DESTDIR}/usr/man/manroff invert.1 > ${DESTDIR}/usr/new/man/cat1/invert.0
#		@echo Installing $(SRCS) in $(DEST)
#		@for i in $(SRCS); do cp $$i $(DEST)/`basename $$i .1`.n; done

print:		$(SRCS)
		@$(PRINT) -man $?
		@touch print

update:;
