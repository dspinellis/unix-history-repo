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

LIBRARY	      = lib$(LIBNAME).a

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
###
_closepdb.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h
closepdb.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h /usr/include/sys/file.h \
	../../../../include/system.h
errpdb.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h
mustopenpdb.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h \
	../../../../include/system.h /usr/include/sys/file.h \
	../../../../include/yesno.h
openpdb.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h \
	../../../../include/system.h /usr/include/sys/file.h \
	../../../../include/yesno.h
paddkey.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h
pbaddflag.o: ../../../../include/pdbuf.h ../../../../include/yesno.h
pbaddkey.o: ../../../../include/pdbuf.h ../../../../include/yesno.h
pbaddstring.o: ../../../../include/pdbuf.h ../../../../include/yesno.h
pbchgkey.o: ../../../../include/null.h ../../../../include/pdbuf.h \
	../../../../include/yesno.h
pbcmpfield.o: ../../../../include/pdbuf.h
pbcmpkey.o: ../../../../include/pdbuf.h
pbcpyfield.o: ../../../../include/pdbuf.h
pbcpykey.o: ../../../../include/pdbuf.h
pbfndflag.o: ../../../../include/pdbuf.h ../../../../include/yesno.h
pbfndkey.o: ../../../../include/null.h ../../../../include/pdbuf.h
pbfndstring.o: ../../../../include/null.h ../../../../include/pdbuf.h
pbgetstring.o: ../../../../include/pdbuf.h
pblenfield.o: ../../../../include/pdbuf.h
pblenkey.o: ../../../../include/pdbuf.h
pbrmflag.o: ../../../../include/pdbuf.h
pbrmkey.o: ../../../../include/pdbuf.h
pbrmstring.o: ../../../../include/pdbuf.h
pbskipfield.o: ../../../../include/pdbuf.h
pbskipkey.o: ../../../../include/pdbuf.h
pbstretch.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h ../../../../include/yesno.h
pchgkey.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h
pfndent.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h \
	../../../../include/yesno.h
pgetent.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h
pputent.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h ../../../../include/yesno.h
pputflag.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h \
	../../../../include/yesno.h
pputstring.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h \
	../../../../include/yesno.h
prment.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h
prmflag.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h
prmkey.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h
prmstring.o: /usr/include/stdio.h ../../../../include/null.h \
	../../../../include/path.h ../../../../include/pdb.h
resetpdb.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h
rewindpdb.o: /usr/include/stdio.h ../../../../include/path.h \
	../../../../include/pdb.h ../../../../include/system.h \
	/usr/include/sys/file.h
