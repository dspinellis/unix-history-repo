# $Header$
#
DEST	      = /usr/new

MAKEFILE      = Makefile

PRINT	      = pr

PROGRAM	      = ptest

SRCS	      = ptest.sh

SUFFIX	      = .sh:s

VERSION	      = V4BSD

all:		$(PROGRAM)

$(PROGRAM):	$(SRCS)
		@echo -n "Loading $(PROGRAM) ... "
		@cp $(SRCS) $(PROGRAM)
		@echo "done"

clean:;

co:;		@co -r$(VERSION) $(HDRS) $(SRCS)

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

diff:;		@rcsdiff -r$(VERSION) $(HDRS) $(SRCS)

index:;

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install $(PROGRAM) $(DEST)

lint:;

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:;

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install tags
