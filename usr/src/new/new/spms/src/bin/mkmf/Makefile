# $Header$
#
CFLAGS	      = -D$(VERSION) -I../../../include -O

DEST	      = $(DESTDIR)/usr/new

EXTHDRS	      = ../../../include/getarg.h \
		../../../include/macro.h \
		../../../include/null.h \
		../../../include/path.h \
		../../../include/slist.h \
		../../../include/spms.h \
		../../../include/system.h \
		../../../include/yesno.h \
		/usr/include/ctype.h \
		/usr/include/signal.h \
		/usr/include/stdio.h \
		/usr/include/sys/dir.h \
		/usr/include/sys/file.h \
		/usr/include/sys/types.h

HDRS	      = Mkmf.h \
		defaultsfx.h \
		hash.h \
		suffix.h \
		target.h

LDFLAGS	      =

LIBS	      = ../../../lib/libslist.a \
		../../../lib/libspms.a

LINKER	      = cc

LINTFLAGS     = -D$(VERSION) -I../../../include

LINTLIST      = ../../../lib/llib-lslist.ln \
		../../../lib/llib-lpdb.ln \
		../../../lib/llib-lspms.ln \
		-lc

MAKEFILE      = Makefile

OBJS	      = Mkmf.o \
		buildlist.o \
		depend.o \
		editmf.o \
		hash.o \
		iolin.o \
		macro.o \
		misc.o \
		suffix.o

PRINT	      = pr

PROGRAM	      = mkmf

SRCS	      = Mkmf.c \
		buildlist.c \
		depend.c \
		editmf.c \
		hash.c \
		iolin.c \
		macro.c \
		misc.c \
		suffix.c

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
###
Mkmf.o: Mkmf.h ../../../include/getarg.h hash.h ../../../include/null.h \
	../../../include/path.h target.h ../../../include/slist.h suffix.h \
	/usr/include/sys/file.h ../../../include/system.h \
	../../../include/yesno.h defaultsfx.h
buildlist.o: /usr/include/sys/types.h /usr/include/sys/dir.h \
	/usr/include/stdio.h Mkmf.h hash.h ../../../include/null.h \
	../../../include/path.h ../../../include/slist.h suffix.h \
	../../../include/system.h /usr/include/sys/file.h \
	../../../include/yesno.h
depend.o: /usr/include/ctype.h /usr/include/stdio.h Mkmf.h hash.h \
	../../../include/macro.h ../../../include/null.h \
	../../../include/path.h ../../../include/slist.h \
	../../../include/system.h /usr/include/sys/file.h \
	../../../include/yesno.h
editmf.o: /usr/include/signal.h /usr/include/stdio.h Mkmf.h hash.h \
	../../../include/macro.h ../../../include/null.h \
	../../../include/slist.h ../../../include/system.h \
	/usr/include/sys/file.h ../../../include/yesno.h
hash.o: ../../../include/null.h hash.h ../../../include/macro.h
iolin.o: /usr/include/stdio.h ../../../include/yesno.h
macro.o: /usr/include/ctype.h /usr/include/stdio.h Mkmf.h hash.h \
	../../../include/macro.h ../../../include/null.h \
	../../../include/slist.h ../../../include/yesno.h
misc.o: /usr/include/ctype.h /usr/include/stdio.h Mkmf.h hash.h \
	../../../include/macro.h ../../../include/null.h \
	../../../include/path.h target.h ../../../include/spms.h \
	../../../include/system.h /usr/include/sys/file.h \
	../../../include/yesno.h
suffix.o: /usr/include/stdio.h Mkmf.h hash.h ../../../include/macro.h \
	../../../include/null.h suffix.h ../../../include/yesno.h
