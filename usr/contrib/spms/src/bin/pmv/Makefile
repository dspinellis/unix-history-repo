# $Header$
#
CFLAGS	      = -D$(VERSION) -I../../../include -O

DEST	      = /usr/new

EXTHDRS	      = ../../../include/bin.h \
		../../../include/macro.h \
		../../../include/null.h \
		../../../include/path.h \
		../../../include/yesno.h

HDRS	      =

LDFLAGS	      =

LIBS	      = ../../../lib/libspms.a \
		../../../lib/libpdb.a \
		../../../lib/libspms.a

LINKER	      = cc

LINTFLAGS     = -D$(VERSION) -I../../../include

LINTLIST      = ../../../lib/llib-lspms.ln \
		../../../lib/llib-lpdb.ln \
		-lc

MAKEFILE      = Makefile

OBJS	      = pmv.o

PRINT	      = pr

PROGRAM	      = pmv

SRCS	      = pmv.c

VERSION	      = V4BSD

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS) pmove

co:;		@co -r$(VERSION) $(HDRS) $(SRCS)

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

diff:;		@rcsdiff -r$(VERSION) $(HDRS) $(SRCS)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DEST)

lint:;	        @lint $(LINTFLAGS) $(SRCS) $(LINTLIST)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install tags
