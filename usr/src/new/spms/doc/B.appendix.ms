.bp
.nr PS 12
.SH
Appendix B.  Project `vs' Makefile Templates
.LP
.nr PS 10
.XS
Appendix B. Project `vs' Makefile Templates
.XE
.nf
.ps 9
.vs 11
.ta 16n
.nr e 16n-\w'= 'u

CFLAGS\h'|\neu'= \-D$(VERSION) \-I../../include \-O

DEST\h'|\neu'= ../../bin

EXTHDRS\h'|\neu'=

HDRS\h'|\neu'= 

LDFLAGS\h'|\neu'=

LIBS\h'|\neu'= ../../lib/libhash.a \\
	../../lib/liblist.a \\
	/usr/lib/libcurses.a \\
	/usr/lib/libtermlib.a

LINKER\h'|\neu'= cc

LINTFLAGS\h'|\neu'= \-D$(VERSION) \-I../../include

LINTLIST\h'|\neu'= ../../lib/llib-lhash.ln \\
	../../lib/llib-llist.ln \\
	\-lc

MAKEFILE\h'|\neu'= Makefile

OBJS\h'|\neu'=

PRINT\h'|\neu'= pr

PROGRAM\h'|\neu'=

SRCS\h'|\neu'=

VERSION\h'|\neu'= V3

all:	$(PROGRAM)

$(PROGRAM):	$(OBJS) $(LIBS)
	@echo \-n "Loading $(PROGRAM) ... "
	@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) \-o $(PROGRAM)
	@echo "done"

clean:;	@rm \-f $(OBJS)

co:;	@co \-r$(VERSION) $(HDRS) $(SRCS)

depend:;	@mkmf \-f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

diff:;	@rcsdiff \-r$(VERSION) $(HDRS) $(SRCS)

index:;	@ctags \-wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
	@echo Installing $(PROGRAM) in $(DEST)
	@install \-s $(PROGRAM) $(DEST)

lint:;	@lint $(LINTFLAGS) $(SRCS) $(LINTLIST)

print:;	@$(PRINT) $(HDRS) $(SRCS)

program:	$(PROGRAM)

tags:	$(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:	$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM):	 $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
	@make \-f $(MAKEFILE) DEST=$(DEST) install tags


.ce
\fIFigure B1.\fR  `p.Makefile' program makefile template
.bp
CFLAGS\h'|\neu'= \-D$(VERSION) \-I../../../include \-O

DEST\h'|\neu'= ../../../lib

EXTHDRS\h'|\neu'=

HDRS\h'|\neu'=

LIBNAME\h'|\neu'=

LIBRARY\h'|\neu'= lib$(LIBNAME).a

LINTFLAGS\h'|\neu'= \-D$(VERSION) \-I../../../include

LINTLIB\h'|\neu'= llib-l$(LIBNAME).ln

LINTLIST\h'|\neu'= \-lc

MAKEFILE\h'|\neu'= Makefile

OBJS\h'|\neu'=

PRINT\h'|\neu'= pr

SRCS\h'|\neu'=

VERSION\h'|\neu'= V3

all:	$(LIBRARY)

$(LIBRARY):	$(OBJS) $(LINTLIB)
	@echo \-n "Loading $(LIBRARY) ... "
	@ar cru $(LIBRARY) $(OBJS)
	@ranlib $(LIBRARY)
	@echo "done"

$(LINTLIB):	$(SRCS) $(HDRS) $(EXTHDRS)
	@echo "Loading $(LINTLIB) ..."
	@lint $(LINTFLAGS) \-C$(LIBNAME) $(SRCS)
	@echo "done"

clean:;	@rm \-f $(OBJS)

co:;	@co \-r$(VERSION) $(HDRS) $(SRCS)

depend:;	@mkmf \-f $(MAKEFILE) LIBRARY=$(LIBRARY) DEST=$(DEST)

diff:;	@rcsdiff \-r$(VERSION) $(HDRS) $(SRCS)

extract:;	@ar xo $(DEST)/$(LIBRARY)
	@rm \-f __.SYMDEF

index:;	@ctags \-wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
	@echo Installing $(LIBRARY) in $(DEST)
	@install $(LIBRARY) $(DEST)
	@ranlib $(DEST)/$(LIBRARY)
	@echo Installing $(LINTLIB) in $(DEST)
	@install $(LINTLIB) $(DEST)

library:	$(LIBRARY)

lint:;	@lint $(LINTFLAGS) $(SRCS) $(LINTLIST)

lintlib:	$(LINTLIB)

print:;	@$(PRINT) $(HDRS) $(SRCS)

tags:	$(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:	$(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY):	 $(SRCS) $(HDRS) $(EXTHDRS)
	@-ar xo $(DEST)/$(LIBRARY)
	@make \-f $(MAKEFILE) DEST=$(DEST) install tags clean


.ce
\fIFigure B2.\fR  `l.Makefile' library makefile template
.fi
.nr PS 10
.ps 10
.vs 12
.TA
