#############################################################################
#
#  Makefile for lf
#


# ------ source locations

# source directory for libg++.a
SRCDIR = ../../src

# source include directory 
SRCIDIR= ../../g++-include

# ------ installation destinations

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
GXXLDFLAGS = -L$(SRCDIR) -lg++ -lm -ltermcap $(OSLDFLAG)

# these flags tell test0 where ld++ and crt1+.o are
TFLAGS = -DLDXX=\"$(LDXX)\" -DCRT1X=\"$(GXXCRT1)\"


# g++ files should have extension .cc
.SUFFIXES: .cc
.cc.o:
	$(GXX) $(GXXFLAGS) -c  $<

#############################################################################
#  If you move this makefile, update the variable below
#  or else depend won't work.
#############################################################################
MAKEFILE	= Makefile
CFILES		= lf.cc entry-handler.cc screen-handler.cc option-handler.cc \
              directory-handler.cc sort.cc Dirent.cc
HFILES		= entry-handler.h screen-handler.h option-handler.h \
              directory-handler.h Dirent.h
OFILES		= lf.o entry-handler.o screen-handler.o option-handler.o \
              directory-handler.o sort.o Dirent.o
PROGRAM		= lf

$(PROGRAM):	$(OFILES)
	$(GXX) $(OFILES) -o $(PROGRAM) $(GXXLDFLAGS) -ltermcap

clean:
	rm -f *.o *.out *~ #*# core

realclean: clean
	rm -f $(PROGRAM)

depend:
	g++dep -f $(MAKEFILE) $(CFILES)

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

Dirent.o : Dirent.cc Dirent.h 
directory-handler.o : directory-handler.cc Dirent.h option-handler.h \
  entry-handler.h screen-handler.h directory-handler.h 
entry-handler.o : entry-handler.cc entry-handler.h screen-handler.h 
lf.o : lf.cc option-handler.h screen-handler.h directory-handler.h \
  entry-handler.h 
option-handler.o : option-handler.cc option-handler.h 
screen-handler.o : screen-handler.cc screen-handler.h 
sort.o : sort.cc 

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
