# $Header$
#
CFLAGS	      = -D$(VERSION) -I../../../../include -O

DEST	      = ../../../../lib

EXTHDRS	      = ../../../../include/bin.h \
		../../../../include/macro.h \
		../../../../include/null.h \
		../../../../include/path.h \
		../../../../include/pdb.h \
		../../../../include/pdbuf.h \
		../../../../include/pdtyp.h \
		../../../../include/pld.h \
		../../../../include/spms.h \
		../../../../include/system.h \
		../../../../include/truefalse.h \
		../../../../include/yesno.h \
		/usr/include/ctype.h \
		/usr/include/machine/machparam.h \
		/usr/include/machine/machparam.h \
		/usr/include/pwd.h \
		/usr/include/signal.h \
		/usr/include/stdio.h \
		/usr/include/sys/dir.h \
		/usr/include/sys/file.h \
		/usr/include/sys/ioctl.h \
		/usr/include/sys/param.h \
		/usr/include/sys/signal.h \
		/usr/include/sys/stat.h \
		/usr/include/sys/ttychars.h \
		/usr/include/sys/ttychars.h \
		/usr/include/sys/ttydev.h \
		/usr/include/sys/ttydev.h \
		/usr/include/sys/types.h \
		/usr/include/sys/types.h

HDRS	      =

LIBNAME	      = spms

LIBRARY	      = libspms.a

LINTFLAGS     = -D$(VERSION) -I../../../../include

LINTLIB	      = llib-l$(LIBNAME).ln

LINTLIST      = -lc

MAKEFILE      = Makefile

OBJS	      = argvtos.o \
		chproject.o \
		diff.o \
		fatal.o \
		getcpd.o \
		getcwp.o \
		gethdir.o \
		getproject.o \
		getpwdir.o \
		getshell.o \
		getwd.o \
		isfg.o \
		isintract.o \
		mk_dir.o \
		mkalias.o \
		mustfopen.o \
		optpath.o \
		pathcat.o \
		pathhead.o \
		pathtail.o \
		pdtmatch.o \
		pdtparse.o \
		pdtyp.o \
		pfxcpy.o \
		ppathcat.o \
		ppathhead.o \
		pperror.o \
		readpath.o \
		readpld.o \
		rm_dir.o \
		strpcpy.o \
		strsav.o \
		warn.o \
		xorpath.o \
		xppath.o \
		yes.o

PRINT	      = pr

SRCS	      = argvtos.c \
		chproject.c \
		diff.c \
		fatal.c \
		getcpd.c \
		getcwp.c \
		gethdir.c \
		getproject.c \
		getpwdir.c \
		getshell.c \
		getwd.c \
		isfg.c \
		isintract.c \
		mk_dir.c \
		mkalias.c \
		mustfopen.c \
		optpath.c \
		pathcat.c \
		pathhead.c \
		pathtail.c \
		pdtmatch.c \
		pdtparse.c \
		pdtyp.c \
		pfxcpy.c \
		ppathcat.c \
		ppathhead.c \
		pperror.c \
		readpath.c \
		readpld.c \
		rm_dir.c \
		strpcpy.c \
		strsav.c \
		warn.c \
		xorpath.c \
		xppath.c \
		yes.c

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
