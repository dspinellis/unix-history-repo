DEST	      = .

EXTHDRS	      =

HDRS	      =

LIBRARY	      = lib.a

MAKEFILE      = Makefile

OBJS	      =

PRINT	      = pr

SRCS	      =

all:		$(LIBRARY)

$(LIBRARY):	$(OBJS)
		@echo -n "Loading $(LIBRARY) ... "
		@ar cru $(LIBRARY) $(OBJS)
		@ranlib $(LIBRARY)
		@echo "done"

clean:;		@rm -f $(OBJS)

depend:;	@mkmf -f $(MAKEFILE) LIBRARY=$(LIBRARY) DEST=$(DEST)

extract:;	@ar xo $(DEST)/$(LIBRARY)
		@rm -f __.SYMDEF

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
		@echo Installing $(LIBRARY) in $(DEST)
		@install $(LIBRARY) $(DEST)
		@ranlib $(DEST)/$(LIBRARY)

library:        $(LIBRARY)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
		@-ar xo $(DEST)/$(LIBRARY)
		@make -f $(MAKEFILE) DEST=$(DEST) install clean
