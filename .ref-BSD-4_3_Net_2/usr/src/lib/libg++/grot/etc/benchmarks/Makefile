# A makefile for the stuff now in libg++/etc/benchmarks

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

# ---------------------------------------------------------------

# select QUICK= -DQUICK to get 50000 instead of 500000 iterations
QUICK= -DQUICK
#QUICK=

all: test_progs

TEST_PROGS=  builtin class virt no_nrv byval call call-var convert virt-var

$(TEST_PROGS) : Int.h Char.h dhrystone.cc

test_progs: $(TEST_PROGS)

run_tests: $(TEST_PROGS)
	@echo "dhrystone with builtin int and char types:"
	@./builtin
	@echo "Using classes Int and Char:"
	@./class
	@echo "Without mixed mode operators (forcing coercions):"
	@./convert
	@echo "Using by-value, rather than by-reference calling conventions:"
	@./byval
	@echo "Without using named return values:"
	@./no_nrv
	@echo "Using calls instead of inline functions:"
	@./call
	@echo "Using calls, with -fthis-is-variable:"
	@./call-var
	@echo "With all member functions virtual (non-inline):"
	@./virt
	@echo "With all member functions virtual, and -fthis-is-variable:"
	@./virt-var
	@echo "(Try other permutations/switches -- See Int.h.)"

builtin:
	$(GXX) $(QUICK) $(GXXFLAGS) -DBUILTIN dhrystone.cc $(GXXLDFLAGS) -o builtin

class:
	$(GXX) $(QUICK) $(GXXFLAGS)  dhrystone.cc $(GXXLDFLAGS) -o class

call:
	$(GXX) $(QUICK) $(GXXFLAGS) -DCALL dhrystone.cc $(GXXLDFLAGS) -o call

call-var:
	$(GXX) $(QUICK) $(GXXFLAGS) -fthis-is-variable -DCALL dhrystone.cc $(GXXLDFLAGS) -o call-var

convert:
	$(GXX) $(QUICK) $(GXXFLAGS) -DCONVERT dhrystone.cc $(GXXLDFLAGS) -o convert

no_nrv:
	$(GXX) $(QUICK) $(GXXFLAGS) -DNO_NRV dhrystone.cc $(GXXLDFLAGS) -o no_nrv

byval:
	$(GXX) $(QUICK) $(GXXFLAGS) -DBYVAL dhrystone.cc $(GXXLDFLAGS) -o byval

virt:
	$(GXX) $(QUICK) $(GXXFLAGS) -DVIRT dhrystone.cc $(GXXLDFLAGS) -o virt

virt-var:
	$(GXX) $(QUICK) $(GXXFLAGS) -fthis-is-variable -DVIRT dhrystone.cc $(GXXLDFLAGS) -o virt-var

always:

clean:
	rm -f *.o core

realclean: clean
	rm -f $(TEST_PROGS)
