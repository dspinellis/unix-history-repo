# $Header: Makefile,v 1.1 85/03/14 15:59:05 nicklin Exp $
#
DEST	      = $(DESTDIR)/usr/new/lib

MAKEFILE      = Makefile

PRINT	      = pr

PROGRAM	      = p.Makefile

SRCS	      = l.Makefile \
		p.Makefile \
		t.Makefile

SUFFIX	      = .Makefile:s

VERSION	      = V4BSD

all:;

clean:;

co:;		@co -r$(VERSION) $(SRCS)

depend:;

diff:;		@rcsdiff -r$(VERSION) $(HDRS) $(SRCS)

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
###
