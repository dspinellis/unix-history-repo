# $Header$
#
DEST	      = $(DESTDIR)/usr/new/lib

MAKEFILE      = Makefile

PRINT	      = pr

PROGRAM	      = phelp.cmd

SRCS	      = phelp.cmd \
		phelp.help

SUFFIX	      = .cmd:s .help:s

VERSION	      = V4BSD

all:;

clean:;

co:;		@co -r$(VERSION) $(SRCS)

depend:;

index:;

install:	$(SRCS)
		@echo Installing $(SRCS) in $(DEST)
		@cp $(SRCS) $(DEST)

lint:;

print:;		@$(PRINT) $(SRCS)

tags:;

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS)
		@make -f $(MAKEFILE) DEST=$(DEST) install tags
