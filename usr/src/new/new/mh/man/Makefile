DEST	      = $(DESTDIR)/usr/man/mann

MAKEFILE      = Makefile

PRINT	      = vtroff

SRCS	      =	comp.1 \
		dist.1 \
		file.1 \
		folder.1 \
		forw.1 \
		inc.1 \
		next.1 \
		pick.1 \
		prev.1 \
		prompter.1 \
		repl.1 \
		rmf.1 \
		rmm.1 \
		scan.1 \
		send.1 \
		show.1

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
