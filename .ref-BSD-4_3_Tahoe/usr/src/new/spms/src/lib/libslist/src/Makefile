# $Header$
#
CFLAGS	      = -D$(VERSION) -I../../../../include -O

DEST	      = ../../../../lib

EXTHDRS	      = ../../../../include/macro.h \
		../../../../include/null.h \
		../../../../include/slist.h \
		../../../../include/slslist.h \
		../../../../include/yesno.h \
		/usr/include/stdio.h

HDRS	      =

LIBNAME	      = slist

LIBRARY	      = libslist.a

LINTFLAGS     = -D$(VERSION) -I../../../../include

LINTLIB	      = llib-l$(LIBNAME).ln

LINTLIST      = -lc

MAKEFILE      = Makefile

OBJS	      = slappend.o \
		slget.o \
		slinit.o \
		slpop.o \
		slprepend.o \
		slprint.o \
		slrewind.o \
		slrm.o \
		slsappend.o \
		slsget.o \
		slsinit.o \
		slsort.o \
		slsplice.o \
		slsprepend.o \
		slsprint.o \
		slsrewind.o \
		slsrm.o \
		slssort.o

PRINT	      = pr

SRCS	      = slappend.c \
		slget.c \
		slinit.c \
		slpop.c \
		slprepend.c \
		slprint.c \
		slrewind.c \
		slrm.c \
		slsappend.c \
		slsget.c \
		slsinit.c \
		slsort.c \
		slsplice.c \
		slsprepend.c \
		slsprint.c \
		slsrewind.c \
		slsrm.c \
		slssort.c

VERSION	      = V4BSD

all:		$(LIBRARY)

$(LIBRARY):	$(OBJS) $(LINTLIB)
		@echo -n "Loading $(LIBRARY) ... "
		@ar cru $(LIBRARY) $(OBJS)
		@ranlib $(LIBRARY)
		@echo "done"

$(LINTLIB):	$(SRCS) $(HDRS) $(EXTHDRS)
		@echo "Loading $(LINTLIB) ..."
		@lint $(LINTFLAGS) -C$(LIBNAME) $(SRCS)
		@echo "done"

clean:;		@rm -f $(OBJS)

co:;		@co -r$(VERSION) $(HDRS) $(SRCS)

depend:;	@mkmf -f $(MAKEFILE) LIBRARY=$(LIBRARY) DEST=$(DEST)

diff:;		@rcsdiff -r$(VERSION) $(HDRS) $(SRCS)

extract:;	@ar xo $(DEST)/$(LIBRARY)
		@rm -f __.SYMDEF

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
		@echo Installing $(LIBRARY) in $(DEST)
		@install $(LIBRARY) $(DEST)
		@ranlib $(DEST)/$(LIBRARY)
		@echo Installing $(LINTLIB) in $(DEST)
		@install $(LINTLIB) $(DEST)

library:        $(LIBRARY)

lint:;		@lint $(LINTFLAGS) $(SRCS) $(LINTLIST)

lintlib:	$(LINTLIB)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
		@-ar xo $(DEST)/$(LIBRARY)
		@make -f $(MAKEFILE) DEST=$(DEST) install tags clean
