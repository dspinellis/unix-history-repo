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
host_makefile_frag = ../../m68k-hp-bsd.mh
ALL=all.internal
# A makefile for the stuff now in libg++/etc/benchmarks

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
docdir = $(datadir)/doc

SHELL = /bin/sh

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = `if [ -f ../../binutils/ar ] ; \
	then echo ../../binutils/ar ; \
	else echo ar ; fi`

AR_FLAGS = qc
RANLIB = `if [ -f ../../binutils/ranlib ] ; \
	then echo ../../binutils/ranlib ; \
	else echo ranlib ; fi`

MAKEINFO = `if [ -f ../../texinfo/C/makeinfo ] ; \
	then echo ../../texinfo/C/makeinfo ; \
	else echo makeinfo ; fi`

IO_DIR = iostream

CC = `if [ -f ../../../gcc/gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -f ../../../gcc/gcc ] ; \
	then echo ../../../gcc/gcc -B../../../gcc/ ; \
	else echo gcc ; fi`
CXXFLAGS= -g -O

XTRAFLAGS = `if [ -f ../../../gcc/gcc ] ; \
	then echo -I../../../gcc/include ; \
	else echo ; fi`
WRAP_C_INCLUDES=-I$(srcdir)/../../g++-include

# select QUICK= -DQUICK to get 50000 instead of 500000 iterations
QUICK= -DQUICK
#QUICK=

# call-var virt-var left out, because gcc passes -fthis-is-variable
# to collect, which tried to pass it to gcc/cc1.
TEST_PROGS=  builtin class virt no_nrv byval call convert # call-var virt-var

LIBS = ../../libg++.a

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

.PHONY: all
all:

$(TEST_PROGS) : Int.h Char.h dhrystone.cc
test_progs: $(TEST_PROGS)

check: $(TEST_PROGS)
	@echo "dhrystone with builtin int and char types:"
	@./builtin
	@echo "Using classes Int and Char:"
	@./class
	@echo "Without mixed mode operators (forcing coercions):"
	@./convert
#	@echo "Using by-value, rather than by-reference calling conventions:"
#	@./byval
	@echo "Without using named return values:"
	@./no_nrv
	@echo "Using calls instead of inline functions:"
	@./call
#	@echo "Using calls, with -fthis-is-variable:"
#	@./call-var
	@echo "With all member functions virtual (non-inline):"
	@./virt
	@echo "With all member functions virtual, and -fthis-is-variable:"
#	@./virt-var
#	@echo "(Try other permutations/switches -- See Int.h.)"

run_tests: check

C_FLAGS = $(CXXFLAGS) $(NOSTDINC) -I. -I../.. -I$(srcdir) \
  $(WRAP_C_INCLUDES) $(XTRAFLAGS)

builtin:
	$(CXX) $(C_FLAGS) -DBUILTIN $(srcdir)/dhrystone.cc $(LIBS) -o $@

class:
	$(CXX) $(C_FLAGS) $(srcdir)/dhrystone.cc $(LIBS) -o $@

call:
	$(CXX) $(C_FLAGS) -DCALL $(srcdir)/dhrystone.cc $(LIBS) -o $@

call-var:
	$(CXX) $(C_FLAGS) -fthis-is-variable -DCALL $(srcdir)/dhrystone.cc $(LIBS) -o $@

convert:
	$(CXX) $(C_FLAGS) -DCONVERT $(srcdir)/dhrystone.cc $(LIBS) -o $@

no_nrv:
	$(CXX) $(C_FLAGS) -DNO_NRV $(srcdir)/dhrystone.cc $(LIBS) -o $@

byval:
	$(CXX) $(C_FLAGS) -DBYVAL $(srcdir)/dhrystone.cc $(LIBS) -o $@

virt:
	$(CXX) $(C_FLAGS) -DVIRT $(srcdir)/dhrystone.cc $(LIBS) -o $@

virt-var:
	$(CXX) $(C_FLAGS) -fthis-is-variable -DVIRT $(srcdir)/dhrystone.cc $(LIBS) -o $@

.PHONY: mostlyclean clean distclean realclean
mostlyclean:
	rm -f *.o \#* core $(TEST_PROGS)
clean: mostlyclean
distclean: clean
	rm -f *~ config.status Makefile
realclean: distclean
	rm -f depend

force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

