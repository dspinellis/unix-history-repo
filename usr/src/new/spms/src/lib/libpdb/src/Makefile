# $Header$
#
CFLAGS	      = -DOPTIMIZE -D$(VERSION) -I../../../../include -O

DEST	      = ../../../../lib

EXTHDRS	      = ../../../../include/null.h \
		../../../../include/path.h \
		../../../../include/pdb.h \
		../../../../include/pdbuf.h \
		../../../../include/system.h \
		../../../../include/yesno.h \
		/usr/include/stdio.h \
		/usr/include/stdio.h \
		/usr/include/sys/file.h

HDRS	      =

LIBNAME	      = pdb

LIBRARY	      = libpdb.a

LINTFLAGS     = -DOPTIMIZE -D$(VERSION) -I../../../../include

LINTLIB	      = llib-l$(LIBNAME).ln

LINTLIST      = -lc

MAKEFILE      = Makefile

OBJS	      = _closepdb.o \
		closepdb.o \
		errpdb.o \
		mustopenpdb.o \
		openpdb.o \
		paddkey.o \
		pbaddflag.o \
		pbaddkey.o \
		pbaddstring.o \
		pbchgkey.o \
		pbclear.o \
		pbcmpfield.o \
		pbcmpkey.o \
		pbcpyfield.o \
		pbcpykey.o \
		pbfndflag.o \
		pbfndkey.o \
		pbfndstring.o \
		pbgetkey.o \
		pbgetstring.o \
		pblenfield.o \
		pblenkey.o \
		pbrmflag.o \
		pbrmkey.o \
		pbrmstring.o \
		pbshrink.o \
		pbskipfield.o \
		pbskipkey.o \
		pbstretch.o \
		pchgkey.o \
		pfndent.o \
		pgetent.o \
		pputent.o \
		pputflag.o \
		pputstring.o \
		prment.o \
		prmflag.o \
		prmkey.o \
		prmstring.o \
		resetpdb.o \
		rewindpdb.o

PRINT	      = pr

SRCS	      = _closepdb.c \
		closepdb.c \
		errpdb.c \
		mustopenpdb.c \
		openpdb.c \
		paddkey.c \
		pbaddflag.c \
		pbaddkey.c \
		pbaddstring.c \
		pbchgkey.c \
		pbclear.c \
		pbcmpfield.c \
		pbcmpkey.c \
		pbcpyfield.c \
		pbcpykey.c \
		pbfndflag.c \
		pbfndkey.c \
		pbfndstring.c \
		pbgetkey.c \
		pbgetstring.c \
		pblenfield.c \
		pblenkey.c \
		pbrmflag.c \
		pbrmkey.c \
		pbrmstring.c \
		pbshrink.c \
		pbskipfield.c \
		pbskipkey.c \
		pbstretch.c \
		pchgkey.c \
		pfndent.c \
		pgetent.c \
		pputent.c \
		pputflag.c \
		pputstring.c \
		prment.c \
		prmflag.c \
		prmkey.c \
		prmstring.c \
		resetpdb.c \
		rewindpdb.c

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
