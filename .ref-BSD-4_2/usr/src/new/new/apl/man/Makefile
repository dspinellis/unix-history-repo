DEST	      = $(DESTDIR)/usr/man/mann

MAKEFILE      = Makefile

PRINT	      = vtroff

SRCS	      = apl.1 \
		aplcvt.1 \
		xed.1

SUFFIX	      = .1:s

all:;

clean:;

depend:;	@mkmf -f $(MAKEFILE)

install:;	@echo Installing $(SRCS) in $(DEST)
		@for i in $(SRCS); do cp $$i $(DEST)/`basename $$i .1`.n; done

print:		$(SRCS)
		@$(PRINT) -man $?
		@touch print

update:;
