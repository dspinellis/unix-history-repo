CFLAGS	      = -O

DEST	      = $(DESTDIR)/usr/ucb

EXTHDRS	      = /usr/include/ctype.h \
		/usr/include/stdio.h \
		/usr/include/sys/stat.h \
		/usr/include/sys/types.h

HDRS	      =

LDFLAGS	      =

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = fsplit.o

PRINT	      = pr

PROGRAM	      = fsplit

SRCS	      = fsplit.c

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS)

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DEST)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install
###
fsplit.o: /usr/include/ctype.h /usr/include/stdio.h \
	/usr/include/sys/types.h /usr/include/sys/stat.h
