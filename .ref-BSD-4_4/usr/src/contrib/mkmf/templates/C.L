CFLAGS	      =

DEST	      = .

EXTHDRS	      =

HDRS	      =

INSTALL	      = cp

LIBRARY	      = lib.a

MAKEFILE      = Makefile

OBJS	      =

PRINT	      = pr

SHELL	      = /bin/sh

SRCS	      =

SYSHDRS	      =

all:		$(LIBRARY)

$(LIBRARY):	$(OBJS)

clean:;		@rm -f core

clobber:;	@rm -f $(LIBRARY) core tags

depend:;	@mkmf -f $(MAKEFILE)

echo:;		@echo $(HDRS) $(SRCS)

extract:;	@ar x $(DEST)/$(LIBRARY)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
		@echo Installing $(LIBRARY) in $(DEST)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) $(LIBRARY) $(DEST)); fi

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)
