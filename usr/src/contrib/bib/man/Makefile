DEST	      = $(DESTDIR)/usr/man/mann

MAKEFILE      = Makefile

PRINT	      = vtroff

SRCS	      = bib.1 \
		invert.1

all:;

clean:;

depend:;	@mkmf -f $(MAKEFILE)

install: bib.0 invert.0
	install -c -o bin -g bin -m 444 bib.0 invert.0 \
	    ${DESTDIR}/usr/new/man/cat1
#		@echo Installing $(SRCS) in $(DEST)
#		@for i in $(SRCS); do cp $$i $(DEST)/`basename $$i .1`.n; done

print:		$(SRCS)
		@$(PRINT) -man $?
		@touch print

update:;
