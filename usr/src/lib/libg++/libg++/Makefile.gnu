# Makefile for libg++.a

# Copyright (C) 1988 Free Software Foundation
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

# make parameters -- these should normally be inherited from parent make

# ------ source locations

# source directory for libg++.a
SRCDIR = ../src

# source include directory 
SRCIDIR= ../g++-include

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
# use this only if you have a strange stdio implementation
#OSFLAG = -DDEFAULT_filebuf

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

GXX_OPTIMIZATION_FLAGS= -O -fstrength-reduce  -felide-constructors -fschedule-insns -fdelayed-branch  -fsave-memoized 

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

###########################################################################
# 
# declarations from here on should not normally need to be changed 
# in order to compile libg++.a
#

# library sources 

OBJS =  AllocRing.o Obstack.o File.o  ostream.o istream.o \
 streambuf.o filebuf.o Filebuf.o \
 PlotFile.o  SFile.o builtin.o \
 regex.o Regex.o String.o  Integer.o Rational.o Complex.o Random.o \
 BitSet.o BitString.o LogNorm.o SmplHist.o SmplStat.o \
 Normal.o NegExp.o Weibull.o Erlang.o DiscUnif.o \
 Uniform.o Poisson.o HypGeom.o Geom.o Binomial.o \
 RNG.o ACG.o MLCG.o  RndInt.o  \
 Fix.o Fix16.o Fix24.o CursesW.o GetOpt.o EH.o EH2.o\
 xyzzy.o gnulib3.o new.o delete.o malloc.o chr.o dtoa.o error.o form.o gcd.o \
 hash.o itoa.o \
 lg.o fmtq.o ioob.o pow.o sqrt.o str.o timer.o bcopy.o \
 std.o ctype.o curses.o math.o compare.o

###########################################################################
#
# compilation actions
#

all: libg++.a

libg++.a: $(OBJS)
	rm -f libg++.a
	$(AR) r libg++.a $(OBJS)
	$(RANLIB) libg++.a

install:
	$(INSTALL) libg++.a $(LIBDIR)/libg++.a
	-if [ -x /usr/bin/$(RANLIB) -o -x /bin/ranlib ] ; then \
		$(RANLIB) $(LIBDIR)/libg++.a; \
	fi

clean:
	rm -f *.o core

realclean: clean
	rm -f libg++.a


###########################################################################
#
#  dependencies
#

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ACG.o : ACG.cc $(SRCIDIR)/ACG.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h 
AllocRing.o : AllocRing.cc $(SRCIDIR)/std.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/AllocRing.h $(SRCIDIR)/new.h 
Binomial.o : Binomial.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/Binomial.h 
BitSet.o : BitSet.cc $(SRCIDIR)/BitSet.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h $(SRCIDIR)/Obstack.h \
  $(SRCIDIR)/AllocRing.h $(SRCIDIR)/new.h 
BitString.o : BitString.cc $(SRCIDIR)/BitString.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h $(SRCIDIR)/Obstack.h \
  $(SRCIDIR)/AllocRing.h $(SRCIDIR)/new.h 
Complex.o : Complex.cc $(SRCIDIR)/Complex.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h 
CursesW.o : CursesW.cc $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/stdarg.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/CursesW.h $(SRCIDIR)/curses.h 
DiscUnif.o : DiscUnif.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/DiscUnif.h 
EH.o : EH.cc $(SRCIDIR)/setjmp.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/streambuf.h 
EH2.o : EH2.c
Erlang.o : Erlang.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/Erlang.h 
File.o : File.cc $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/stdarg.h $(SRCIDIR)/sys/file.h \
  $(SRCIDIR)/sys/types.h 
Filebuf.o : Filebuf.cc $(SRCIDIR)/streambuf.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/sys/file.h \
  $(SRCIDIR)/sys/types.h 
Fix.o : Fix.cc $(SRCIDIR)/Fix.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h $(SRCIDIR)/Integer.h \
  $(SRCIDIR)/Obstack.h $(SRCIDIR)/AllocRing.h 
Fix16.o : Fix16.cc $(SRCIDIR)/Fix16.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h 
Fix24.o : Fix24.cc $(SRCIDIR)/Fix24.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h 
Geom.o : Geom.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/Geom.h 
GetOpt.o : GetOpt.cc $(SRCIDIR)/GetOpt.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/stdio.h 
HypGeom.o : HypGeom.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/HypGeom.h 
Integer.o : Integer.cc $(SRCIDIR)/Integer.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h $(SRCIDIR)/ctype.h \
  $(SRCIDIR)/Obstack.h $(SRCIDIR)/AllocRing.h \
  $(SRCIDIR)/new.h 
MLCG.o : MLCG.cc $(SRCIDIR)/MLCG.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h 
Normal.o : Normal.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h 
NegExp.o : NegExp.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/NegExp.h
Obstack.o : Obstack.cc $(SRCIDIR)/values.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/Obstack.h 
PlotFile.o : PlotFile.cc $(SRCIDIR)/PlotFile.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
Poisson.o : Poisson.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/Poisson.h 
RNG.o : RNG.cc $(SRCIDIR)/values.h \
  $(SRCIDIR)/assert.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/RNG.h 
Rational.o : Rational.cc $(SRCIDIR)/Rational.h \
  $(SRCIDIR)/Integer.h $(SRCIDIR)/stream.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/streambuf.h 
SFile.o : SFile.cc $(SRCIDIR)/SFile.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
SmplHist.o : SmplHist.cc $(SRCIDIR)/stream.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/streambuf.h \
  $(SRCIDIR)/SmplHist.h \
  $(SRCIDIR)/SmplStat.h 
SmplStat.o : SmplStat.cc $(SRCIDIR)/stream.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/streambuf.h \
  $(SRCIDIR)/SmplStat.h
String.o : String.cc $(SRCIDIR)/String.h \
  $(SRCIDIR)/stream.h $(SRCIDIR)/File.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/streambuf.h $(SRCIDIR)/ctype.h \
  $(SRCIDIR)/new.h $(SRCIDIR)/regex.h 
Uniform.o : Uniform.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/Uniform.h 
Weibell.o : Weibell.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/Random.h \
  $(SRCIDIR)/RNG.h $(SRCIDIR)/assert.h \
  $(SRCIDIR)/Weibull.h 
chr.o : chr.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/AllocRing.h 
dtoa.o : dtoa.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/AllocRing.h 
error.o : error.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
filebuf.o : filebuf.cc $(SRCIDIR)/streambuf.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/sys/file.h \
  $(SRCIDIR)/sys/types.h 
form.o : form.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/stdarg.h \
  $(SRCIDIR)/AllocRing.h 
gcd.o : gcd.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
hash.o : hash.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
istream.o : istream.cc $(SRCIDIR)/stream.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/streambuf.h \
  $(SRCIDIR)/stdarg.h $(SRCIDIR)/ctype.h \
  $(SRCIDIR)/Obstack.h 
itoa.o : itoa.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/AllocRing.h 
lg.o : lg.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
fmtq.o : fmtq.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/AllocRing.h 
ioob.o : ioob.cc $(SRCIDIR)/Obstack.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/stdio.h 
new.o : new.cc $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/malloc.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h 
ostream.o : ostream.cc $(SRCIDIR)/stream.h \
  $(SRCIDIR)/File.h $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/streambuf.h \
  $(SRCIDIR)/stdarg.h $(SRCIDIR)/ctype.h \
  $(SRCIDIR)/Obstack.h 
pow.o : pow.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
regex.o : regex.cc $(SRCIDIR)/std.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/malloc.h $(SRCIDIR)/regex.h 
sqrt.o : sqrt.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h 
str.o : str.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/AllocRing.h 
streambuf.o : streambuf.cc $(SRCIDIR)/streambuf.h \
  $(SRCIDIR)/builtin.h $(SRCIDIR)/stddef.h \
  $(SRCIDIR)/std.h $(SRCIDIR)/stdio.h \
  $(SRCIDIR)/math.h $(SRCIDIR)/values.h \
  $(SRCIDIR)/File.h 
timer.o : timer.cc $(SRCIDIR)/builtin.h \
  $(SRCIDIR)/stddef.h $(SRCIDIR)/std.h \
  $(SRCIDIR)/stdio.h $(SRCIDIR)/math.h \
  $(SRCIDIR)/values.h $(SRCIDIR)/osfcn.h \
  $(SRCIDIR)/time.h $(SRCIDIR)/sys/types.h \
  $(SRCIDIR)/sys/socket.h  $(SRCIDIR)/sys/resource.h
xyzzy.o : xyzzy.cc 

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
