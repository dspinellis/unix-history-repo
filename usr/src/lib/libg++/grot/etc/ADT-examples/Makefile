# A makefile for the stuff now in libg++/etc/ADT-examples

# ------ source locations

# source directory for libg++.a
SRCDIR = ../../../src

# source include directory 
SRCIDIR= ../../../g++-include

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
.SUFFIXES: .cc
.cc.o:
	$(GXX) $(GXXFLAGS) -c  $<

TEST_PROGS=  generic-q search keyhash \
      tsort tsortinp genkey patricia kmp 

all: $(TEST_PROGS)

run_tests: $(TEST_PROGS)
	-./generic-q < ./generic-q.cc
	-./tsortinp 20 | ./tsort
	-./keyhash < ./keyhash.cc
	-./search 1000
	-./genkey 1000 200 > input
	-./patricia ./input ./input | grep -v "is found"

#	-./dhrystone

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

clean:
	rm -f *.o core input

realclean: clean
	rm -f $(TEST_PROGS)
