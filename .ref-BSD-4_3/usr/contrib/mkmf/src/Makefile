# $Header: Makefile,v 1.1 84/09/14 15:37:40 nicklin Exp $
#
CFLAGS	      = -D$(VERSION) -O

DEST	      = $(DESTDIR)/usr/new

EXTHDRS	      = /usr/include/ctype.h \
		/usr/include/signal.h \
		/usr/include/stdio.h \
		/usr/include/sys/dir.h \
		/usr/include/sys/file.h \
		/usr/include/sys/types.h

HDRS	      = Mkmf.h \
		defaultrul.h \
		defaultsfx.h \
		dlist.h \
		getarg.h \
		hash.h \
		macro.h \
		null.h \
		path.h \
		rule.h \
		sccsid.h \
		slist.h \
		spms.h \
		suffix.h \
		system.h \
		target.h \
		yesno.h

LDFLAGS	      =

LIBS	      =

LINKER	      = cc

LINTFLAGS     = -D$(VERSION)

LINTLIST      = -lc

MAKEFILE      = Makefile

OBJS	      = Mkmf.o \
		badopt.o \
		buildlist.o \
		depend.o \
		dlist.o \
		editmf.o \
		getcwp.o \
		getproject.o \
		hash.o \
		iolin.o \
		macro.o \
		misc.o \
		mustfopen.o \
		optpath.o \
		pathcat.o \
		pathhead.o \
		pperror.o \
		rule.o \
		slappend.o \
		slinit.o \
		slsort.o \
		strpcpy.o \
		strsav.o \
		suffix.o \
		usage.o \
		warn.o \
		warn2.o \
		warns.o

PRINT	      = pr

PROGRAM	      = mkmf

SRCS	      = Mkmf.c \
		badopt.c \
		buildlist.c \
		depend.c \
		dlist.c \
		editmf.c \
		getcwp.c \
		getproject.c \
		hash.c \
		iolin.c \
		macro.c \
		misc.c \
		mustfopen.c \
		optpath.c \
		pathcat.c \
		pathhead.c \
		pperror.c \
		rule.c \
		slappend.c \
		slinit.c \
		slsort.c \
		strpcpy.c \
		strsav.c \
		suffix.c \
		usage.c \
		warn.c \
		warn2.c \
		warns.c

VERSION	      = V4BSD

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS)

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
