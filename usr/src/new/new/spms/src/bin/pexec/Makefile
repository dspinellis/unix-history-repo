# $Header$
#
CFLAGS	      = -D$(VERSION) -I../../../include -O

DEST	      = $(DESTDIR)/usr/new

EXTHDRS	      = ../../../include/bin.h \
		../../../include/getarg.h \
		../../../include/hash.h \
		../../../include/macro.h \
		../../../include/null.h \
		../../../include/path.h \
		../../../include/pdb.h \
		../../../include/pdtyp.h \
		../../../include/pld.h \
		../../../include/slist.h \
		../../../include/slslist.h \
		../../../include/spms.h \
		../../../include/system.h \
		../../../include/truefalse.h \
		../../../include/yesno.h \
		/usr/include/ctype.h \
		/usr/include/machine/param.h \
		/usr/include/signal.h \
		/usr/include/stdio.h \
		/usr/include/sys/file.h \
		/usr/include/sys/param.h \
		/usr/include/sys/types.h

HDRS	      = pdlist.h \
		pdset.h

LDFLAGS	      =

LIBS	      = ../../../lib/libhash.a \
		../../../lib/libslist.a \
		../../../lib/libspms.a \
		../../../lib/libpdb.a \
		../../../lib/libspms.a

LINKER	      = cc

LINTFLAGS     = -D$(VERSION) -I../../../include

LINTLIST      = ../../../lib/llib-lhash.ln \
		../../../lib/llib-lslist.ln \
		../../../lib/llib-lspms.ln \
		../../../lib/llib-lpdb.ln \
		-lc

MAKEFILE      = Makefile

OBJS	      = Pexec.o \
		exec.o \
		execcmd.o \
		misc.o \
		pdlist.o \
		pdset.o

PRINT	      = pr

PROGRAM	      = pexec

SRCS	      = Pexec.c \
		exec.c \
		execcmd.c \
		misc.c \
		pdlist.c \
		pdset.c

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

lint:;		@lint $(LINTFLAGS) $(SRCS) $(LINTLIST)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

strip:;		@strip $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install tags
###
Pexec.o: ../../../include/bin.h ../../../include/macro.h \
	../../../include/getarg.h ../../../include/null.h \
	../../../include/path.h ../../../include/pdtyp.h \
	../../../include/slist.h ../../../include/spms.h \
	../../../include/yesno.h
exec.o: /usr/include/stdio.h ../../../include/macro.h \
	../../../include/null.h ../../../include/path.h \
	../../../include/pdb.h pdlist.h ../../../include/pld.h \
	../../../include/slslist.h ../../../include/spms.h \
	../../../include/yesno.h
execcmd.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/stdio.h \
	/usr/include/machine/param.h /usr/include/sys/types.h \
	/usr/include/sys/param.h ../../../include/bin.h \
	../../../include/path.h /usr/include/sys/file.h \
	../../../include/system.h ../../../include/yesno.h
misc.o: /usr/include/stdio.h ../../../include/macro.h \
	../../../include/yesno.h
pdlist.o: ../../../include/null.h pdlist.h ../../../include/yesno.h
pdset.o: /usr/include/stdio.h ../../../include/hash.h \
	../../../include/macro.h ../../../include/null.h \
	../../../include/path.h ../../../include/pdb.h pdset.h pdlist.h \
	../../../include/pdtyp.h ../../../include/pld.h \
	../../../include/slist.h ../../../include/slslist.h \
	../../../include/spms.h ../../../include/truefalse.h \
	../../../include/yesno.h
