###############################################################################
#   Instructions to Make, for compilation of UNIX compatibility library
###############################################################################


###############################################################################
#
# This package was generously supplied by Doug Gwyn.  It is a public domain
# implementation of the SVR3 directory access routines.
#
# I have slightly edited the package to produce a stand-alone library
# (normally it updates libc.a).  The reason for this, is that the package
# is used primarily for ISODE's ftam responder, and it's too much heartburn
# to ask someone to reload libc.a just for that (sorry, doug!)
#
# I've also prepended a "_" to the names to avoid any conflicts with routines
# which might already be installed (especially for lint)
#
###############################################################################

LIBES	=	libdirent.a $(TOPDIR)libcompat.a
LLIBS   =
HFILES	=	$(HDIR)usr.dirent.h $(HDIR)sys.dirent.h


##################################################################
# Here it is...
##################################################################

all:		libdirent testdir
inst-all:	# inst-libdirent manuals
install:	install
lint:		l-libdirent l-testdir


################################################################
# libdirent
################################################################

CFILES	=	closedir.c getdents.c opendir.c readdir.c rewinddir.c \
		seekdir.c telldir.c getcwd.c scandir.c
OFILES	=	closedir.o getdents.o opendir.o readdir.o rewinddir.o \
		seekdir.o telldir.o getcwd.o scandir.o


inst-libdirent:	$(LIBDIR)libdirent.a $(LINTDIR)llib-ldirent

$(LIBDIR)libdirent.a:	libdirent.a
		-rm -f $@
		cp libdirent.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-ldirent:	llib-ldirent
		-cp $@ zllib-ldirent
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-ldirent | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libdirent:	libdirent.a

libdirent.a:	direntvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			direntvrsn.o
		-@rm -f $(TOPDIR)libdirent.a $(TOPDIR)llib-ldirent
		-@$(LN) libdirent.a $(TOPDIR)libdirent.a
		-@$(LN) llib-ldirent $(TOPDIR)llib-ldirent
		-@ls -l $@
		-@echo "DIRENT library built normally"

direntvrsn.c:	$(OFILES)
		@$(UTILDIR)version.sh dirent > $@

l-libdirent:;	$(LINT) $(LFLAGS) $(CFILES) direntvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

closedir.o:	$(HFILES)
getdents.o:	$(HFILES) $(HDIR)config.h
opendir.o:	$(HFILES)
readdir.o:	$(HFILES)
rewinddir.o:	$(HFILES)
seekdir.o:	$(HFILES)
telldir.o:	$(HFILES)
getcwd.o:	$(HFILES)
scandir.o:	$(HFILES)


################################################################
# testdir
################################################################

testdir:	testdir.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ testdir.o $(LIBES) $(LSOCKET)

testdir.o:	$(HFILES)
		$(CC) $(CFLAGS) -c $*.c

l-testdir:;	$(LINT) $(LFLAGS) testdir.c llib-ldirent \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# manual pages
################################################################

MANUALS	=	directory.3c dirent.4 getdents.2

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a z* _* core testdir direntvrsn.c

grind:;		iprint Makefile
		tgrind -lc $(CFILES) direntvrsn.c testdir.c
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve
