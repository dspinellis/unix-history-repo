CFLAGS	      = -w -DCOPYCODE

DEST	      = $(DESTDIR)/lib

EXTHDRS	      = /usr/include/ctype.h \
		/usr/include/stdio.h

HDRS	      = c2.h

LDFLAGS	      =

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = c20.o \
		c21.o \
		c22.o

PRINT	      = pr

PROGRAM	      = f2

SRCS	      = c20.c \
		c21.c \
		c22.c

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
c20.o: c2.h /usr/include/stdio.h /usr/include/ctype.h
c21.o: c2.h /usr/include/stdio.h /usr/include/ctype.h
c22.o: c2.h
