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
# Makefile for g++ library test files

# Copyright (C) 1988, 1992 Free Software Foundation
#   written by Doug Lea (dl@rocky.oswego.edu)

# This file is part of GNU CC.

# GNU CC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY.  No author or distributor
# accepts responsibility to anyone for the consequences of using it
# or for whether it serves any particular purpose or works at all,
# unless he says so in writing.  Refer to the GNU CC General Public
# License for full details.

# Everyone is granted permission to copy, modify and redistribute
# GNU CC, but only under the conditions described in the
# GNU CC General Public License.   A copy of this license is
# supposed to have been given to you along with GNU CC so you
# can know your rights and responsibilities.  It should be in a
# file named COPYING.  Among other things, the copyright notice
# and this notice must be preserved on all copies.

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

CC = `if [ -f ../../gcc/gcc ] ; \
	then echo ../../gcc/gcc -B../../gcc/ ; \
	else echo cc ; fi`
CXX = `if [ -f ../../gcc/gcc ] ; \
	then echo ../../gcc/gcc -B../../gcc/ ; \
	else echo gcc ; fi`

CFLAGS = -g
CXXFLAGS = -g -O
XTRAFLAGS = `if [ -f ../../gcc/gcc ] ; \
	then echo -I../../gcc/include ; \
	else echo ; fi`

GENCLASS = ../genclass/genclass
PROTODIR = $(srcdir)/../src/gen
LIBS = ../libg++.a
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

.SUFFIXES: .o .cc

.cc.o:
	$(CXX) $(CXXFLAGS) $(NOSTDINC) -I. -I.. -I$(srcdir)/../$(IO_DIR) \
	-I$(srcdir)/../src $(WRAP_C_INCLUDES) $(XTRAFLAGS) -c $<

# compilation notes:
# The following g++ warnings are expected to appear:
#
# 2. Several warnings from genclass about classes with only .h, no .cc files
#
# 3. A few harmless reminder warnings that some classes have no data members.
#
# 4. Many harmless warnings that arguments to vector-delete are unnecessary
#    when deleting arrays of builtin types.

# test files
TSRCS =  tFile.cc tObstack.cc tString.cc \
 tInteger.cc tRational.cc \
 tComplex.cc tBitSet.cc tBitString.cc tRandom.cc tList.cc tPlex.cc \
 tLList.cc tVec.cc tStack.cc tQueue.cc tDeque.cc tPQ.cc tSet.cc  tBag.cc \
 tMap.cc tFix.cc tFix16.cc tFix24.cc \
 tCurses.cc tGetOpt.cc \
 tiLList.cc

# executables
TOUTS =  test_h $(TEST0) tCurses\
  tFile tObstack tString tInteger \
  tRational tComplex tBitSet tBitString \
  tRandom tFix tFix16 tFix24 tGetOpt \
  tList tPlex tLList tVec \
  tStack tQueue tDeque tPQ tSet tBag tMap 

# files for archived prototype classes
LOBJS = \
 iList.o iSLList.o iDLList.o iVec.o iAVec.o \
 iPlex.o  iFPlex.o  iXPlex.o iRPlex.o iMPlex.o \
 iSet.o iBag.o iMap.o iPQ.o \
 iXPSet.o  iOXPSet.o  iSLSet.o  iOSLSet.o  iBSTSet.o iCHNode.o \
 iAVLSet.o  iSplayNode.o iSplaySet.o  iVHSet.o  iVOHSet.o  iCHSet.o \
 iXPBag.o  iOXPBag.o  iSLBag.o  iOSLBag.o  iSplayBag.o iVHBag.o  iCHBag.o \
 iVHMap.o  iCHMap.o  iSplayMap.o  iAVLMap.o iRAVLMap.o \
 iSplayPQ.o  iPHPQ.o  iXPPQ.o \
 iVStack.o iVQueue.o iStack.o iQueue.o iDeque.o \
 iXPStack.o iSLStack.o iXPQueue.o  iSLQueue.o iXPDeque.o iDLDeque.o

LSRCS = \
 iList.cc iSLList.cc iDLList.cc iVec.cc iAVec.cc \
 iPlex.cc  iFPlex.cc  iXPlex.cc iRPlex.cc iMPlex.cc \
 iSet.cc iBag.cc iMap.cc iPQ.cc \
 iXPSet.cc  iOXPSet.cc  iSLSet.cc  iOSLSet.cc  iBSTSet.cc iCHNode.cc \
 iAVLSet.cc  iSplayNode.cc iSplaySet.cc  iVHSet.cc  iVOHSet.cc  iCHSet.cc \
 iXPBag.cc iOXPBag.cc iSLBag.cc iOSLBag.cc  iSplayBag.cc iVHBag.cc iCHBag.cc \
 iVHMap.cc  iCHMap.cc  iSplayMap.cc  iAVLMap.cc iRAVLMap.cc \
 iSplayPQ.cc  iPHPQ.cc  iXPPQ.cc \
 iVStack.cc iVQueue.cc iStack.cc iQueue.cc iDeque.cc \
 iXPStack.cc iSLStack.cc iXPQueue.cc  iSLQueue.cc iXPDeque.cc iDLDeque.cc

DEPEND_SOURCES = $(srcdir)/*.cc $(LSRCS)

LHDRS =  idefs.h 

.PHONY: all
all:

.PHONY: info
info:
.PHONY: install-info
install-info:
.PHONY: clean-info
clean-info:

.PHONY: check
check: tests

.PHONY: check-tFile check-tObstack check-tString check-tInteger \
  check-tRational check-tComplex check-tBitSet check-tBitString \
  check-tRandom check-tFix check-tFix16 check-tFix24 check-tGetOpt \
  check-tList check-tPlex check-tLList check-tVec \
  check-tStack check-tQueue check-tDeque check-tPQ \
  check-tSet check-tBag check-tMap

check-tFile: tFile $(srcdir)/tFile.inp
	./tFile < $(srcdir)/tFile.inp > tFile.out 2>&1
	diff -b $(srcdir)/tFile.exp tFile.out
check-tObstack: tObstack $(srcdir)/tObstack.inp
	./tObstack < $(srcdir)/tObstack.inp > tObstack.out 2>&1
	diff -b $(srcdir)/tObstack.exp tObstack.out
check-tString: tString $(srcdir)/tString.inp
	./tString < $(srcdir)/tString.inp > tString.out 2>&1
	diff -b $(srcdir)/tString.exp tString.out
check-tInteger: tInteger $(srcdir)/tInteger.inp
	./tInteger < $(srcdir)/tInteger.inp > tInteger.out 2>&1
	diff -b $(srcdir)/tInteger.exp tInteger.out
check-tRational: tRational $(srcdir)/tRational.inp
	./tRational < $(srcdir)/tRational.inp > tRational.out 2>&1
	diff -b $(srcdir)/tRational.exp tRational.out
check-tComplex: tComplex $(srcdir)/tComplex.inp
	./tComplex < $(srcdir)/tComplex.inp > tComplex.out 2>&1
	diff -b $(srcdir)/tComplex.exp tComplex.out
check-tBitSet: tBitSet $(srcdir)/tBitSet.inp
	./tBitSet < $(srcdir)/tBitSet.inp > tBitSet.out 2>&1
	diff -b $(srcdir)/tBitSet.exp tBitSet.out
check-tBitString: tBitString $(srcdir)/tBitString.inp
	./tBitString < $(srcdir)/tBitString.inp > tBitString.out 2>&1
	diff -b $(srcdir)/tBitString.exp tBitString.out
check-tRandom: tRandom $(srcdir)/tRandom.inp
	./tRandom < $(srcdir)/tRandom.inp > tRandom.out 2>&1
	diff -b $(srcdir)/tRandom.exp tRandom.out
check-tFix: tFix $(srcdir)/tFix.inp
	./tFix < $(srcdir)/tFix.inp > tFix.out 2>&1
	diff -b $(srcdir)/tFix.exp tFix.out
check-tFix16: tFix16 $(srcdir)/tFix16.inp
	./tFix16 < $(srcdir)/tFix16.inp > tFix16.out 2>&1
	diff -b $(srcdir)/tFix16.exp tFix16.out
check-tFix24: tFix24 $(srcdir)/tFix24.inp
	./tFix24 < $(srcdir)/tFix24.inp > tFix24.out 2>&1
	diff -b $(srcdir)/tFix24.exp tFix24.out
check-tList: tList $(srcdir)/tList.inp
	./tList < $(srcdir)/tList.inp > tList.out 2>&1
	diff -b $(srcdir)/tList.exp tList.out
check-tPlex: tPlex $(srcdir)/tPlex.inp
	./tPlex < $(srcdir)/tPlex.inp > tPlex.out 2>&1
	diff -b $(srcdir)/tPlex.exp tPlex.out
check-tLList: tLList $(srcdir)/tLList.inp
	./tLList < $(srcdir)/tLList.inp > tLList.out 2>&1
	diff -b $(srcdir)/tLList.exp tLList.out
check-tVec: tVec $(srcdir)/tVec.inp
	./tVec < $(srcdir)/tVec.inp > tVec.out 2>&1
	diff -b $(srcdir)/tVec.exp tVec.out
check-tStack: tStack $(srcdir)/tStack.inp
	./tStack < $(srcdir)/tStack.inp > tStack.out 2>&1
	diff -b $(srcdir)/tStack.exp tStack.out
check-tQueue: tQueue $(srcdir)/tQueue.inp
	./tQueue < $(srcdir)/tQueue.inp > tQueue.out 2>&1
	diff -b $(srcdir)/tQueue.exp tQueue.out
check-tDeque: tDeque $(srcdir)/tDeque.inp
	./tDeque < $(srcdir)/tDeque.inp > tDeque.out 2>&1
	diff -b $(srcdir)/tDeque.exp tDeque.out
check-tPQ: tPQ $(srcdir)/tPQ.inp
	./tPQ < $(srcdir)/tPQ.inp > tPQ.out 2>&1
	diff -b $(srcdir)/tPQ.exp tPQ.out
check-tSet: tSet $(srcdir)/tSet.inp
	./tSet < $(srcdir)/tSet.inp > tSet.out 2>&1
	diff -b $(srcdir)/tSet.exp tSet.out
check-tBag: tBag $(srcdir)/tBag.inp
	./tBag < $(srcdir)/tBag.inp > tBag.out 2>&1
	diff -b $(srcdir)/tBag.exp tBag.out
check-tMap: tMap $(srcdir)/tMap.inp
	./tMap < $(srcdir)/tMap.inp > tMap.out 2>&1
	diff -b $(srcdir)/tMap.exp tMap.out
check-tGetOpt: tGetOpt $(srcdir)/tGetOpt.inp
	./tGetOpt -abc -de10 -2000 -h3i \
	  <$(srcdir)/tGetOpt.inp >tGetOpt.out 2>&1
	diff -b $(srcdir)/tGetOpt.exp tGetOpt.out

$(TOUTS): $(LIBGXX)

LIBTEST=libtest.a

# We don't do check-tRandom, because it is not portable.

# Comment this out if your compiler doesn't handle templates:
CHECK_TEMPLATES=check-templates

tests checktests: clean_tests test_h tCurses \
  check-tFile check-tObstack check-tString check-tInteger \
  check-tRational check-tComplex check-tBitSet check-tBitString \
  check-tFix check-tFix16 check-tFix24 check-tGetOpt \
  check-tList check-tPlex check-tLList check-tVec \
  check-tStack check-tQueue check-tDeque check-tPQ \
  check-tSet check-tBag check-tMap $(CHECK_TEMPLATES)
	./test_h
	echo "(Must run tCurses manually from tty)"

check-templates: check-tiLList

# Build all the tests, but don't run them. (Useful when cross-compiling.)

make-tests: test_h tCurses \
  tFile tObstack tString tInteger tRational tComplex tBitSet tBitString \
  tFix tFix16 tFix24 tGetOpt tList tPlex tLList tVec \
  tStack tQueue tDeque tPQ tSet tBag tMap \
  tiLList

test_h: test_h.o
	$(CXX) $(LDFLAGS) test_h.o -o $@ $(LIBS) -lm
	

	
$(LIBTEST): $(LHDRS) $(LOBJS)
	rm -f $(LIBTEST)
	$(AR) r $(LIBTEST) $(LOBJS)
	$(RANLIB) $(LIBTEST)

#
# other tests
#

tFile:  tFile.o
	$(CXX) $(LDFLAGS) tFile.o -o $@ $(LIBS)

# tiLList.cc is the template version of tList.cc.

tiLList:  tiLList.o
	$(CXX) $(LDFLAGS) tiLList.o -o $@ $(LIBS)
check-tiLList: tiLList $(srcdir)/tLList.inp
	./tiLList < $(srcdir)/tLList.inp > tiLList.out 2>&1
	diff -b $(srcdir)/tLList.exp tiLList.out

tObstack:  tObstack.o
	$(CXX) $(LDFLAGS) tObstack.o -o $@ $(LIBS)

tString:  tString.o
	$(CXX) $(LDFLAGS) tString.o -o $@ $(LIBS)

tInteger:  tInteger.o
	$(CXX) $(LDFLAGS) tInteger.o -o $@ $(LIBS)

twrapper:  twrapper.o
	$(CXX) $(LDFLAGS) twrapper.o -o $@ $(LIBS)

tgwrapper:   tgwrapper.o
	$(CXX) $(LDFLAGS) tgwrapper.o -o $@ $(LIBS)

tRational:  tRational.o
	$(CXX) $(LDFLAGS) tRational.o -o $@ $(LIBS) -lm

tComplex:  tComplex.o
	$(CXX) $(LDFLAGS) tComplex.o -o $@ $(LIBS) -lm

tBitSet:  tBitSet.o
	$(CXX) $(LDFLAGS) tBitSet.o -o $@ $(LIBS)

tBitString:  tBitString.o
	$(CXX) $(LDFLAGS) tBitString.o -o $@ $(LIBS)

tRandom:  tRandom.o
	$(CXX) $(LDFLAGS) tRandom.o -o $@ $(LIBS) -lm

tFix:  tFix.o
	$(CXX) $(LDFLAGS) tFix.o -o $@ $(LIBS) -lm

tFix16:  tFix16.o
	$(CXX) $(LDFLAGS) tFix16.o -o $@ $(LIBS) -lm

tFix24:  tFix24.o
	$(CXX) $(LDFLAGS) tFix24.o -o $@ $(LIBS) -lm

tCurses:  tCurses.o
	$(CXX) $(LDFLAGS) tCurses.o  -o $@ $(LIBS) -lcurses -ltermcap 

tGetOpt:  tGetOpt.o
	$(CXX) $(LDFLAGS) tGetOpt.o -o $@ $(LIBS)

tList:  $(LIBTEST)  tList.o
	$(CXX) $(LDFLAGS) tList.o -o $@ $(LIBTEST) $(LIBS)

tPlex:  $(LIBTEST)  tPlex.o
	$(CXX) $(LDFLAGS) tPlex.o -o $@ $(LIBTEST) $(LIBS)

tLList:  $(LIBTEST) tLList.o
	$(CXX) $(LDFLAGS) tLList.o -o $@ $(LIBTEST) $(LIBS)

tVec: $(LIBTEST) tVec.o
	$(CXX) $(LDFLAGS) tVec.o -o $@ $(LIBTEST) $(LIBS)

tStack: $(LIBTEST) tStack.o
	$(CXX) $(LDFLAGS) tStack.o -o $@ $(LIBTEST) $(LIBS)

tQueue: $(LIBTEST) tQueue.o
	$(CXX) $(LDFLAGS) tQueue.o -o $@ $(LIBTEST) $(LIBS)

tDeque: $(LIBTEST) tDeque.o
	$(CXX) $(LDFLAGS) tDeque.o -o $@ $(LIBTEST) $(LIBS)

tPQ: $(LIBTEST) tPQ.o
	$(CXX) $(LDFLAGS) tPQ.o -o $@ $(LIBTEST) $(LIBS)

tSet: $(LIBTEST) tSet.o
	$(CXX) $(LDFLAGS) tSet.o -o $@ $(LIBTEST) $(LIBS)

tBag:  $(LIBTEST) tBag.o
	$(CXX) $(LDFLAGS) tBag.o -o $@ $(LIBTEST) $(LIBS)

tMap:  $(LIBTEST) tMap.o
	$(CXX) $(LDFLAGS) tMap.o -o $@ $(LIBTEST) $(LIBS)

idefs.h:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val defs i
iSet.h iSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val Set i
iXPSet.h iXPSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val XPSet i
iOXPSet.h iOXPSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val OXPSet i
iSLSet.h iSLSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SLSet i
iOSLSet.h iOSLSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val OSLSet i
iBSTSet.h iBSTSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val BSTSet i
iAVLSet.h iAVLSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val AVLSet i
iSplayNode.h iSplayNode.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SplayNode i
iSplaySet.h iSplaySet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SplaySet i
iVHSet.h iVHSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val VHSet i
iVOHSet.h iVOHSet.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val VOHSet i
iCHSet.h iCHSet.cc : 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val CHSet i
iCHNode.h iCHNode.cc : 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val CHNode i
iBag.h iBag.cc : 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val Bag i
iXPBag.h iXPBag.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val XPBag i
iOXPBag.h iOXPBag.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val OXPBag i
iSLBag.h iSLBag.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SLBag i
iOSLBag.h iOSLBag.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val OSLBag i
iSplayBag.h iSplayBag.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SplayBag i
iVHBag.h iVHBag.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val VHBag i
iCHBag.h iCHBag.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val CHBag i
iMap.h iMap.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) -2 int val int val Map i
iVHMap.h iVHMap.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) -2 int val int val VHMap i
iCHMap.h iCHMap.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) -2 int val int val CHMap i
iSplayMap.h iSplayMap.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) -2 int val int val SplayMap i
iAVLMap.h iAVLMap.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) -2 int val int val AVLMap i
iRAVLMap.h iRAVLMap.cc :
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) -2 int val int val RAVLMap i
iPQ.h iPQ.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val PQ i
iPHPQ.h iPHPQ.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val PHPQ i
iSplayPQ.h iSplayPQ.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SplayPQ i
iXPPQ.h iXPPQ.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val XPPQ i
iStack.h iStack.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val Stack i
iQueue.h iQueue.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val Queue i
iDeque.h iDeque.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val Deque i
iSLStack.h iSLStack.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SLStack i
iSLQueue.h iSLQueue.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SLQueue i
iDLDeque.h iDLDeque.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val DLDeque i
iList.h iList.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val List i
iPlex.h iPlex.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val Plex i
iFPlex.h iFPlex.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val FPlex i
iXPlex.h iXPlex.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val XPlex i
iMPlex.h iMPlex.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val MPlex i
iRPlex.h iRPlex.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val RPlex i
iFPStack.h iFPStack.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val FPStack i
iXPStack.h iXPStack.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val XPStack i
iFPQueue.h iFPQueue.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val FPQueue i
iXPQueue.h iXPQueue.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val XPQueue i
iXPDeque.h iXPDeque.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val XPDeque i
iSLList.h iSLList.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val SLList i
iDLList.h iDLList.cc: 
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val DLList i
iVec.h iVec.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val Vec i
iAVec.h iAVec.cc:
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val AVec i
iVStack.h iVStack.cc: iStack.h
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val VStack i
iVQueue.h iVQueue.cc: iQueue.h
	PROTODIR=$(PROTODIR); export PROTODIR; $(GENCLASS) int val VQueue i

relink: force
	rm -f $(TOUTS)

.PHONY: mostlyclean clean distclean realclean clean_tests
clean_tests: force
	rm -f *.out
mostlyclean: clean_tests
	rm -f *.o \#* core trie-gen
	rm -f ffile streamfile i*.cc i*.h CXX.hack*
	rm -f test.bye test.bye2 test.shell $(LIBTEST) $(TOUTS) 
clean: mostlyclean
distclean: clean
	rm -f *~ config.status Makefile $(TRASH_FILES)
realclean: distclean
	rm -f depend

force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

.NOEXPORT:


# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

tBag.o : tBag.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iBag.h \
  iXPBag.h \
  iSLBag.h \
  iVHBag.h \
  iCHBag.h \
  iOXPBag.h \
  iOSLBag.h \
  iSplayBag.h
tBitSet.o : tBitSet.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tBitString.o : tBitString.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tComplex.o : tComplex.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tCurses.o : tCurses.cc 
tDeque.o : tDeque.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iDeque.h \
  iXPDeque.h \
  iDLDeque.h
tFile.o : tFile.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  $(srcdir)/../$(IO_DIR)/strstream.h \
  $(srcdir)/../$(IO_DIR)/SFile.h \
  $(srcdir)/../$(IO_DIR)/fstream.h \
  $(srcdir)/../$(IO_DIR)/PlotFile.h 
tFix.o : tFix.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tFix16.o : tFix16.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tFix24.o : tFix24.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tGetOpt.o : tGetOpt.cc 
tInteger.o : tInteger.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tLList.o : tLList.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iSLList.h \
  iDLList.h
tList.o : tList.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iList.h 
tMap.o : tMap.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iMap.h \
  iSplayMap.h \
  iVHMap.h \
  iCHMap.h \
  iAVLMap.h \
  iRAVLMap.h
tObstack.o : tObstack.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tPQ.o : tPQ.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iPQ.h \
  iXPPQ.h \
  iPHPQ.h \
  iSplayPQ.h
tPlex.o : tPlex.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iPlex.h \
  iFPlex.h \
  iXPlex.h \
  iMPlex.h \
  iRPlex.h
tQueue.o : tQueue.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iQueue.h \
  iXPQueue.h \
  iVQueue.h \
  iSLQueue.h
tRandom.o : tRandom.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tRational.o : tRational.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tSet.o : tSet.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iSet.h \
  iXPSet.h \
  iSLSet.h \
  iVHSet.h \
  iVOHSet.h \
  iCHSet.h \
  iOXPSet.h \
  iOSLSet.h \
  iBSTSet.h \
  iAVLSet.h \
  iSplaySet.h
tStack.o : tStack.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iStack.h \
  iXPStack.h \
  iVStack.h \
  iSLStack.h
tString.o : tString.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h 
tVec.o : tVec.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  iVec.h \
  iAVec.h 
test_h.o : test_h.cc \
  $(srcdir)/../$(IO_DIR)/stream.h \
  $(srcdir)/../$(IO_DIR)/iostream.h \
  $(srcdir)/../$(IO_DIR)/streambuf.h \
  $(srcdir)/../$(IO_DIR)/istream.h \
  $(srcdir)/../$(IO_DIR)/ostream.h 

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
