# This file was generated automatically by configure.  Do not edit.
VPATH = .
links = 
host_alias = m68k-hp-bsd
host_cpu = m68k
host_vendor = hp
host_os = bsd
host_canonical = m68k-hp-bsd
target_alias = m68k-hp-bsd
target_cpu = m68k
target_vendor = hp
target_os = bsd
target_canonical = m68k-hp-bsd
host_makefile_frag = ../m68k-hp-bsd.mh
ALL=all.internal
VERSION=0.90
SUBDIRS = stdio test

srcdir = .

prefix = /usr/local

exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib

datadir = $(prefix)/lib
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info
includedir = $(prefix)/include
gxx_includedir = $(libdir)/g++-include
docdir = $(datadir)/doc

SHELL = /bin/sh

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = `if [ -f ../../../binutils/ar ] ; \
	then echo ../../../binutils/ar ; \
	else echo ar ; fi`

AR_FLAGS = qc
RANLIB = `if [ -f ../../../binutils/ranlib ] ; \
	then echo ../../../binutils/ranlib ; \
	else echo ranlib ; fi`

MAKEINFO = `if [ -f ../../../texinfo/C/makeinfo ] ; \
	then echo ../../../texinfo/C/makeinfo ; \
	else echo makeinfo ; fi`

#CC = `if [ -f $${rootprefix}../../gcc/gcc ] ; \
#	then echo $${rootprefix}../../gcc/gcc -B$${rootprefix}../../gcc/ ; \
#	else echo cc ; fi`
#CXX = `if [ -f $${rootprefix}../../gcc/gcc ] ; \
#	then echo $${rootprefix}../../gcc/gcc -B$${rootprefix}../../gcc/ ; \
#	else echo gcc ; fi`

#CC = `[ $(build_host) != $(host_alias) ] && host_prefix=$(host_alias)-;\
#	if [ -f $${rootprefix}../..$(host_root)/$${host_prefix}gcc/gcc ] ; \
#	then echo $${rootprefix}../..$(host_root)/$${host_prefix}gcc/gcc -B$${rootprefix}../..$(host_root)/$${host_prefix}gcc/; \
#	else echo $${host_prefix}gcc; fi`

CC = /usr/src/devel/gcc.2/gcc-2.3.3/xgcc -B/usr/src/devel/gcc.2/gcc-2.3.3/

CXX = $(CC)

XTRAFLAGS = `if [ -f $${rootprefix}../..$(host_root)/$(host_prefix)gcc/gcc ] ;\
	then echo -I$${rootprefix}../..$(host_root)/$(host_prefix)gcc/include;\
	else echo ; fi`
WRAP_C_INCLUDES=-I$(srcdir)/../g++-include

#### host and target dependent Makefile fragments come in here.
INSTALL = install -c
INSTALL_DATA = install -c -m 644
# RANLIB = ranlib
# Use -nostdinc++ flag if your version of gcc understands it.
NOSTDINC = # -nostdinc++

# If the C include files are C++-ready (with extern "C"),
# define: HAVE_CPLUS_EXTERN = 1 and: WRAP_C_INCLUDES =
# If not, define HAVE_CPLUS_EXTERN = 0, and do not define WRAP_C_INCLUDES here.
HAVE_CPLUS_EXTERN = 0

# Flags to pass to gen-params when building _G_config.h.
# For example: G_CONFIG_ARGS = size_t="unsigned long"
G_CONFIG_ARGS =
##

# List of variables to pass to sub-makes.  This should not be needed
# by GNU make or Sun make (both of which pass command-line variable
# overrides thouh $(MAKE)) but may be needed by older versions.

FLAGS_TO_PASS= \
	"prefix=$(prefix)" \
	"exec_prefix=$(exec_prefix)" \
	"tooldir=$(tooldir)" \
	"AR=$(AR)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"RANLIB=$(RANLIB)" \
	"LOADLIBES=$(LOADLIBES)" \
	"LDFLAGS=$(LDFLAGS)" \
	"MAKEINFO=$(MAKEINFO)" \
	"XTRAFLAGS=$(XTRAFLAGS)"

.SUFFIXES: .o .C

.C.o:
	$(CXX) $(CXXFLAGS) $(NOSTDINC) -I. -I.. -I$(srcdir) -I$(srcdir)/../src $(WRAP_C_INCLUDES) -c $(XTRAFLAGS) $<

TARGETLIB=libio.a

IOSTREAM_OBS= streambuf.o stdstrbufs.o iostream.o stdstreams.o strstream.o \
  indstream.o PlotFile.o SFile.o fstream.o parsestream.o stream.o makebuf.o \
  editbuf.o filebuf.o sgetline.o igetline.o igetsb.o procbuf.o \
  sbufvform.o sbufvscan.o stdiostream.o floatconv.o outfloat.o iomanip.o


# The following include files are merely to provide partial
# compatibilty with the old stream library from libg++.

COMPAT_INCLUDES = istream.h ostream.h stream.h

# The following include files are publicly visible, and
# should be installed where user programs can find them.

USER_INCLUDES = editbuf.h fstream.h iomanip.h iostream.h indstream.h \
  parsestream.h streambuf.h strstream.h PlotFile.h SFile.h procbuf.h \
  stdiostream.h $(COMPAT_INCLUDES)

# The following include files are private to the implementation.

INTERNAL_INCLUDES = ioprivate.h floatio.h

DEPEND_SOURCES = $(srcdir)/*.C

#NOSTDINC = -nostdinc++
CFLAGS = -g
CXXFLAGS = -g -O

.PHONY: all
all: $(TARGETLIB)
	@rootprefix=`pwd`/; export rootprefix; \
	$(MAKE) subdir_do "DODIRS=$(SUBDIRS)" DO=all $(FLAGS_TO_PASS)

.PHONY: install
install:
.PHONY: check
check: $(TARGETLIB)
	@rootprefix=`pwd`/; export rootprefix; \
	$(MAKE) subdir_do "DODIRS=$(SUBDIRS)" DO=check $(FLAGS_TO_PASS)
.PHONY: info
info: iostream.info
.PHONY: clean-info
clean-info:
.PHONY: install-info
install-info: info
	-if [ -d $(infodir) ] ; then true ; else mkdir $(infodir) ; fi
	for i in *.info* ; do \
		$(INSTALL_DATA) $$i $(infodir)/$$i ; \
		chmod a-x $(infodir)/$$i ; \
	done

.PHONY: boltcc
boltcc:
	$(MAKE) $(FLAGS_TO_PASS) $(TARGETLIB)

# ../libg++.a depends on the stamp file.
stamp: $(IOSTREAM_OBS)
	@touch stamp

$(TARGETLIB): stamp
	-rm -f $(TARGETLIB)
	$(AR) $(AR_FLAGS) $(TARGETLIB) $(IOSTREAM_OBS)
	$(RANLIB) $(TARGETLIB)

DIST_NAME=iostream-${VERSION}
DIST_FILE=$(DIST_NAME).tar.Z
${DIST_FILE}: dist
.PHONY: dist
dist: FORCE
	-mkdir $(DIST_NAME)
	cd $(DIST_NAME); \
	  ln -s $(srcdir)/../Make.defs .; \
	  ln -s $(srcdir)/../g++-include/_G_config.h .; \
	  ln -s $(srcdir)/* \
	    $(srcdir)/../../configure $(srcdir)/../../config.sub .; \
	  rm -rf test stdio *~ CVS configure.in; \
	  sed -e 's|\.\./||' <$(srcdir)/configure.in > configure.in; \
	  mkdir test stdio
	cd $(DIST_NAME)/stdio; \
	  ln -s $(srcdir)/stdio/* .; \
	  rm -rf *~ CVS configure.in; \
	  sed -e 's|\.\./||' <$(srcdir)/stdio/configure.in > configure.in
	cd $(DIST_NAME)/test; \
	  ln -s $(srcdir)/test/* .; \
	  rm -rf *~ CVS configure.in; \
	  sed -e 's|\.\./||' <$(srcdir)/test/configure.in > configure.in
	tar -c -h -f - $(DIST_NAME) | compress -c >$(DIST_NAME).tar.Z
	rm -rf $(DIST_NAME)

# clean rules

#.PHONY: mostlyclean clean distclean realclean
#.PHONY: do_mostlyclean do_clean do_distclean do_realclean
do_mostlyclean:
	rm -f *.o core *.tar.Z stamp
do_clean: do_mostlyclean
	rm -f *.a
do_distclean: do_clean
#	rm -f *~ *.a config.status *.info Makefile
	rm -f *~ *.a config.status *.info
do_realclean: do_distclean
#	rm -f depend

#mostlyclean: do_mostlyclean
#	@$(MAKE) subdir_do "DODIRS=$(SUBDIRS)" DO=mostlyclean
#clean: do_clean
#	@$(MAKE) subdir_do "DODIRS=$(SUBDIRS)" DO=clean
distclean:
	@$(MAKE) subdir_do "DODIRS=$(SUBDIRS)" DO=distclean
	@$(MAKE) do_distclean
#realclean:
#	@$(MAKE) subdir_do "DODIRS=$(SUBDIRS)" DO=realclean
#	@$(MAKE) do_realclean
#
#iostream.info: $(srcdir)/iostream.texi
#	$(MAKEINFO) -o iostream.info $(srcdir)/iostream.texi

.PHONY: install-include-files
install-include-files:
	[ -d $(gxx_includedir) ] || mkdir $(gxx_includedir)
	cd $(srcdir); \
	for FILE in $(USER_INCLUDES) ; do \
		rm -f $(gxx_includedir)/$$FILE ; \
		$(INSTALL_DATA) $$FILE $(gxx_includedir)/$$FILE ; \
		chmod a-x $(gxx_includedir)/$$FILE ; \
	done


.PHONY: subdir_do
subdir_do: force
	@rootprefix=`pwd`/; export rootprefix; \
	for i in $(DODIRS); do \
		if [ -d ./$$i ] ; then \
			echo "cd $$i; make $(DO) ..." ; \
			(cd $$i ; $(MAKE) $(DO) $(FLAGS_TO_PASS)) || exit 1 ; \
		else true ; fi ; \
	done

.PHONY: force
force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

.NOEXPORT:


# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

PlotFile.o : PlotFile.C \
  PlotFile.h \
  fstream.h \
  iostream.h \
  streambuf.h 
SFile.o : SFile.C \
  SFile.h \
  fstream.h \
  iostream.h \
  streambuf.h 
editbuf.o : editbuf.C \
  ioprivate.h \
  streambuf.h \
  editbuf.h \
  fstream.h \
  iostream.h \
  streambuf.h
filebuf.o : filebuf.C \
  ioprivate.h \
  streambuf.h 
fstream.o : fstream.C \
  ioprivate.h \
  streambuf.h \
  fstream.h \
  iostream.h \
  streambuf.h
igetline.o : igetline.C \
  iostream.h \
  streambuf.h 
igetsb.o : igetsb.C \
  ioprivate.h \
  streambuf.h \
  iostream.h \
  streambuf.h
indstream.o : indstream.C \
  indstream.h \
  iostream.h \
  streambuf.h 
iostream.o : iostream.C \
  ioprivate.h \
  streambuf.h \
  iostream.h \
  streambuf.h \
  floatio.h
makebuf.o : makebuf.C \
  ioprivate.h \
  streambuf.h \
  fstream.h \
  iostream.h \
  streambuf.h 
parsestream.o : parsestream.C \
  ioprivate.h \
  streambuf.h \
  parsestream.h
procbuf.o : procbuf.C \
  ioprivate.h \
  streambuf.h \
  procbuf.h \
  streambuf.h 
sbufvform.o : sbufvform.C \
  ioprivate.h \
  streambuf.h \
  floatio.h
sbufvscan.o : sbufvscan.C \
  ioprivate.h \
  streambuf.h \
  floatio.h
sgetline.o : sgetline.C \
  ioprivate.h \
  streambuf.h 
stdstrbufs.o : stdstrbufs.C \
  ioprivate.h \
  streambuf.h 
stdstreams.o : stdstreams.C \
  ioprivate.h \
  streambuf.h \
  iostream.h \
  streambuf.h
stream.o : stream.C \
  ioprivate.h \
  streambuf.h \
  stream.h \
  iostream.h \
  streambuf.h \
  strstream.h
streambuf.o : streambuf.C \
  ioprivate.h \
  streambuf.h 
strstream.o : strstream.C \
  ioprivate.h \
  streambuf.h \
  strstream.h \
  iostream.h \
  streambuf.h
stdiostream.o : stdiostream.C \
  stdiostream.h \
  streambuf.h
dtoa.o : dtoa.C \
  ioprivate.h \
  streambuf.h
outfloat.o : outfloat.C \
  ioprivate.h \
  streambuf.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
