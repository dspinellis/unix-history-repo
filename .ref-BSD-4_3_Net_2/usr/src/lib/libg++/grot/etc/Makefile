# A makefile for the stuff now in libg++/etc

# ------ source locations

# source directory for libg++.a
SRCDIR = ../../src

# source include directory 
SRCIDIR= ../../g++-include

# the genclass program
GENCLASS=../../genclass
# and its prototypes directory
PROTODIR=../../g++-include

# ------ installation destinations
# ------ You will require write-permission on the destination directories
# ------ in order to `make install'


prefix =/usr/gnu

# libg++.a destination
LIBDIR = $(prefix)/lib

# executables directory: location to install the genclass class generator
BINDIR = $(prefix)/bin

# location to install include file directory
IDIR = $(prefix)/lib/g++-include


# ------- System-dependent defines
# ------- use the second form of each for SystemV (USG)

# g++ flags
OSFLAG=
#OSFLAG = -DUSG

# ld or ld++ flags
OSLDFLAG =
#OSLDFLAG= -lPW

# how to install
INSTALL=install -c
#INSTALL=cp

# ranlib if necessary
RANLIB=ranlib
#RANLIB=echo

# which make?
MAKE=make

#which ar?
AR=ar


# not used, but convenient for those who preprocess things while compiling
SHELL=/bin/sh


# ------ compiler names

# GNU C++ compiler name
GXX = g++
#GXX=gcc

# GNU CC compiler name (needed for some .c files in libg++.a)
CC = gcc

# GNU loader
LDXX = $(LIBDIR)/gcc-ld++

# crt0+.o location (for dynamic loading tests)
GXXCRT1=$(LIBDIR)/crt1+.o

# ------ Other compilation flags
# ------ modify as you like -- the ones here are sheer overkill

GXX_OPTIMIZATION_FLAGS= -O -fstrength-reduce  -felide-constructors -fschedule-insns -fdelayed-branch -fsave-memoized 

GCC_OPTIMIZATION_FLAGS= -O -fstrength-reduce -fdelayed-branch 

DEBUG_FLAGS= -g

#use this only if you like to look at lots of useless messages
VERBOSITY_FLAGS= -Wall -v

GXX_INCLUDE_DIRS= -I$(SRCIDIR)

GCC_INCLUDE_DIRS= -I$(prefix)/lib/gcc-include -I/usr/include -I$(SRCIDIR)

PIPE_AS= -pipe

# Flags for all C++ compiles
GXXFLAGS = $(OSFLAG) $(GXX_INCLUDE_DIRS) $(DEBUG_FLAGS) $(GXX_OPTIMIZATION_FLAGS) $(VERBOSITY_FLAGS) $(PIPE_AS)

# Flags for all C compiles
CFLAGS= $(OSFLAG) $(GCC_INCLUDE_DIRS) $(DEBUG_FLAGS) $(GCC_OPTIMIZATION_FLAGS) $(VERBOSITY_FLAGS) $(PIPE_AS)

# g++ load time flags 
GXXLDFLAGS = -L$(SRCDIR) -lg++ -lm $(OSLDFLAG)

# these flags tell test0 where ld++ and crt1+.o are
TFLAGS = -DLDXX=\"$(LDXX)\" -DCRT1X=\"$(GXXCRT1)\"


# g++ files should have extension .cc
.SUFFIXES: .cc .c .o
.cc.o:
	$(GXX) $(GXXFLAGS) -c  $<


PROGS=  etags fib

all: $(PROGS) graph ADT-examples benchmarks lf PlotFile3D trie-gen

run_tests: $(PROGS)
	-./fib 1000
	-cd graph; $(MAKE) run_tests
	-cd ADT-examples; $(MAKE) run_tests
	-cd benchmarks; $(MAKE) run_tests
	-cd PlotFile3D; $(MAKE) run_tests
	-cd trie-gen; $(MAKE) run_tests

etags: etags.o
	$(CC) $(CFLAGS) etags.o -o etags

etags.o: etags.c
	$(CC) $(CFLAGS) -DETAGS -c etags.c

fib: fib.o
	$(GXX)  fib.o $(GXXLDFLAGS) -o fib

generic-q: generic-q.o
	$(GXX)  generic-q.o $(GXXLDFLAGS) -o generic-q

tsort: tsortinp tsort.o
	$(GXX)  tsort.o $(GXXLDFLAGS) -o tsort


tsortinp: tsortinp.o
	$(GXX)  tsortinp.o $(GXXLDFLAGS) -o tsortinp

keyhash: keyhash.o
	$(GXX)  keyhash.o $(GXXLDFLAGS) -o keyhash

search: search.o
	$(GXX)  search.o $(GXXLDFLAGS) -o search

genkey: genPatkey.o
	$(GXX)  genPatkey.o $(GXXLDFLAGS) -o genkey 
   
Patricia.o: Patricia.h
Patmain.o: Patricia.h 

patricia: Patmain.o Patricia.o
	$(GXX)  Patmain.o Patricia.o $(GXXLDFLAGS) -o patricia

kmp: kmp.o
	$(GXX)  kmp.o $(GXXLDFLAGS) -o kmp

graph: always
	(cd graph;	$(MAKE)  GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" GENCLASS=$(GENCLASS) PROTODIR=$(PROTODIR))

trie-gen: always
	(cd trie-gen;	$(MAKE) GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)")

ADT-examples: always
	(cd ADT-examples;	$(MAKE) GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)")  

benchmarks: always
	(cd benchmarks;	$(MAKE) GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)")  

PlotFile3D: always
	(cd PlotFile3D;	$(MAKE)  GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)")

lf: always
	(cd lf;	$(MAKE)  GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)")

install: etags g++dep
	$(INSTALL) etags $(BINDIR)/etags
	$(INSTALL) g++dep $(BINDIR)/g++dep

always:

clean:
	rm -f *.o core input C++.hack* etags
	cd graph; make clean
	cd PlotFile3D; make clean
	cd trie-gen; make clean
	cd benchmarks; make clean
	cd ADT-examples; make clean
	cd lf; make clean

realclean:
	rm -f *.o core input C++.hack* etags $(PROGS)
	cd graph; make realclean
	cd PlotFile3D; make realclean
	cd trie-gen; make realclean
	cd benchmarks; make realclean
	cd ADT-examples; make realclean
	cd lf; make realclean

.PHONY:	graph PlotFile3D trie-gen benchmarks ADT-examples lf
