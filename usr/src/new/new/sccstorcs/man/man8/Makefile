DEST	      = $(DESTDIR)/usr/man/mann

MAKEFILE      = Makefile

PRINT	      = vtroff

SRCS	      = sccstorcs.8

SUFFIX	      = .8:s

all:;

clean:;

depend:;	@mkmf -f $(MAKEFILE)

install:;	@echo Installing $(SRCS) in $(DEST)
		@for i in $(SRCS); do cp $$i $(DEST)/`basename $$i .8`.n; done

print:		$(SRCS)
		@$(PRINT) -man $?
		@touch print

update:;
