.bp
.nr PS 12
.SH
Appendix A.  Standard Makefile Templates
.nr PS 10
.LP
.XS
Appendix A. Standard Makefile Templates
.XE
.nf
.ps 9
.vs 11
.ta 16n
.nr e 16n-\w'= 'u

DEST\h'|\neu'= .

EXTHDRS\h'|\neu'=

HDRS\h'|\neu'=

LDFLAGS\h'|\neu'=

LIBS\h'|\neu'=

LINKER\h'|\neu'= cc

MAKEFILE\h'|\neu'= Makefile

OBJS\h'|\neu'=

PRINT\h'|\neu'= pr

PROGRAM\h'|\neu'= a.out

SRCS\h'|\neu'=

all:	$(PROGRAM)

$(PROGRAM):	$(OBJS) $(LIBS)
	@echo \-n "Loading $(PROGRAM) ... "
	@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) \-o $(PROGRAM)
	@echo "done"

clean:;	@rm \-f $(OBJS)

depend:;	@mkmf \-f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;	@ctags \-wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
	@echo Installing $(PROGRAM) in $(DEST)
	@install \-s $(PROGRAM) $(DEST)

print:;	@$(PRINT) $(HDRS) $(SRCS)

program:	$(PROGRAM)

tags:	$(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:	$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM):	 $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
	@make \-f $(MAKEFILE) DEST=$(DEST) install


.ce
\fIFigure A1.\fR  `p.Makefile' program makefile template
.bp
DEST\h'|\neu'= .

EXTHDRS\h'|\neu'=

HDRS\h'|\neu'=

LIBRARY\h'|\neu'= lib.a

MAKEFILE\h'|\neu'= Makefile

OBJS\h'|\neu'=

PRINT\h'|\neu'= pr

SRCS\h'|\neu'=

all:	$(LIBRARY)

$(LIBRARY):	$(OBJS)
	@echo \-n "Loading $(LIBRARY) ... "
	@ar cru $(LIBRARY) $(OBJS)
	@ranlib $(LIBRARY)
	@echo "done"

clean:;	@rm \-f $(OBJS)

depend:;	@mkmf \-f $(MAKEFILE) LIBRARY=$(LIBRARY) DEST=$(DEST)

extract:;	@ar xo $(DEST)/$(LIBRARY)
	@rm \-f __.SYMDEF

index:;	@ctags \-wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
	@echo Installing $(LIBRARY) in $(DEST)
	@install $(LIBRARY) $(DEST)
	@ranlib $(DEST)/$(LIBRARY)

library:	$(LIBRARY)

print:;	@$(PRINT) $(HDRS) $(SRCS)

tags:	$(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:	$(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY):	 $(SRCS) $(HDRS) $(EXTHDRS)
	@-ar xo $(DEST)/$(LIBRARY)
	@make \-f $(MAKEFILE) DEST=$(DEST) install clean


.ce
\fIFigure A2.\fR  `l.Makefile' library makefile template
.fi
.nr PS 10
.ps 10
.vs 12
.TA
