# Makefile for GNU C++ class library (libg++)
#   Copyright (C) 1989 Free Software Foundation, Inc.
#   written by Doug Lea (dl@rocky.oswego.edu)

#This file is part of GNU libg++.

#GNU libg++ is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 1, or (at your option)
#any later version.

#GNU libg++ is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GNU libg++; see the file COPYING.  If not, write to
#the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

###########################################################################
#
# Directories, paths, compilation flags and program names.
#
# Please make sure these are correct.
#

# ------ source locations

# Manually set PWD to *this* directory if you are not using gnu make
PWD := $(shell pwd)
#PWD=/home/dl/libg++

# source include directory 
SRCIDIR= $(PWD)/g++-include

# the genclass program
GENCLASS=$(PWD)/genclass
# and its directory of prototype files
PROTODIR=$(PWD)/g++-include/gen

# ------ installation destinations
# ------ You will require write-permission on the destination directories
# ------ in order to `make install'

# set `prefix' to something else if you want to install things
# in nonstandard places

prefix =/usr/gnu

# libg++.a destination
LIBDIR = $(prefix)/lib

# executables directory: location to install the genclass class generator
BINDIR = $(prefix)/bin

# directory to install man pages
MANDIR= $(prefix)/man

# location to install include file directory
IDIR = $(prefix)/lib/g++-include


# ------- System-dependent defines
# ------- use the second form of each for SystemV (USG)

# g++ flags
OSFLAG=
#OSFLAG = -DUSG

# other compilation control flags -- use any combination

# use this only if you have a strange stdio implementation
#XTRAFLAGS = -DDEFAULT_filebuf

# use this if you do not want gnulib3 in libg++.a
#XTRAFLAGS = -DNO_GNULIB3

# use this if you need COFF encapulation defined in gnulib3
#XTRAFLAGS = -DCOFF_ENCAPSULATE

# use this if you want to disable line buffering for stream output
#XTRAFLAGS = -DNO_LINE_BUFFER_STREAMBUF

# Use this to disable placing libg++ version of malloc in libg++.a 
#XTRAFLAGS = -DNO_LIBGXX_MALLOC

# Please use this & send me some results of malloc_stats() sometime
# (it is off by default, since stat gathering hurts performance)
#XTRAFLAGS = -DMALLOC_STATS

#suggested for NeXT by cdr@acc.stolaf.edu
#XTRAFLAGS = -DNO_GNULIB3 -DNO_LIBGXX_MALLOC

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
LDXX = $(LIBDIR)/gcc-ld
#LDXX = $(LIBDIR)/gcc-ld++

# crt0+.o location (for dynamic loading tests)
GXXCRT1=$(LIBDIR)/crt1+.o

# ------ Other compilation flags
# ------ modify as you like -- the ones here are sheer overkill
# ------ However, You MUST compile libg++.a with EITHER -O OR
# ------ -DUSE_LIBGXX_INLINES or both

GXX_OPTIMIZATION_FLAGS= -O -fstrength-reduce  -felide-constructors -fschedule-insns -fdelayed-branch -fsave-memoized 

#GXX_OPTIMIZATION_FLAGS=-DUSE_LIBGXX_INLINES

GCC_OPTIMIZATION_FLAGS= -O -fstrength-reduce -fdelayed-branch 

DEBUG_FLAGS= -g

#use this only if you like to look at lots of useless messages
#VERBOSITY_FLAGS= -Wall -v 
VERBOSITY_FLAGS= -Wall

GXX_INCLUDE_DIRS= -I$(SRCIDIR)

GCC_INCLUDE_DIRS= -I$(prefix)/lib/gcc-include -I/usr/include -I$(SRCIDIR)

#use this only if you use GNU as (gas) or other assemblers that 
#can read from pipes. 
PIPE_AS= -pipe
#PIPE_AS=

# Flags for all C++ compiles
GXXFLAGS = $(OSFLAG) $(GXX_INCLUDE_DIRS) $(DEBUG_FLAGS) $(GXX_OPTIMIZATION_FLAGS) $(VERBOSITY_FLAGS) $(XTRAFLAGS) $(PIPE_AS)

# Flags for all C compiles
CFLAGS= $(OSFLAG) $(GCC_INCLUDE_DIRS) $(DEBUG_FLAGS) $(GCC_OPTIMIZATION_FLAGS) $(VERBOSITY_FLAGS) $(XTRAFLAGS) $(PIPE_AS)

# g++ load time flags 
GXXLDFLAGS = -L$(PWD)/src -lg++ -lm $(OSLDFLAG)

# Comment out the next line to disable incremental linking test
# (this test NOT included in 1.39.0, so don't re-enable)
#TEST0=test0
TEST0=

###########################################################################
#
# compilation actions
#




src: FORCE
	(cd src; $(MAKE) GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" VPATH="$(SRCIDIR)" AR="$(AR)" PROTODIR="$(PROTODIR)" GENCLASS="$(GENCLASS)")

tests: FORCE
	(cd tests;	$(MAKE) checktests GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)" TEST0="$(TEST0)" PROTODIR="$(PROTODIR)" GENCLASS="$(GENCLASS)")

etc: FORCE
	(cd etc;	$(MAKE) GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)" PROTODIR="$(PROTODIR)" GENCLASS="$(GENCLASS)")

run_etc: FORCE
	(cd etc;	$(MAKE) run_tests GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" MAKE="$(MAKE)" prefix="$(prefix)" AR="$(AR)" PROTODIR="$(PROTODIR)" GENCLASS="$(GENCLASS)")

gperf: FORCE
	(cd gperf;	$(MAKE) GXX="$(GXX)"  GXXFLAGS="$(GXXFLAGS)" GXXLDFLAGS="$(GXXLDFLAGS)" LIBDIR="$(LIBDIR)" SRCIDIR="$(SRCIDIR)" CC="$(CC)" CFLAGS="$(CFLAGS)" RANLIB="$(RANLIB)" LDXX="$(LDXX)" GXXCRT1="$(GXXCRT1)" prefix="$(prefix)" AR="$(AR)" PROTODIR="$(PROTODIR)" GENCLASS="$(GENCLASS)")

genclass: genclass.sh
	echo "/^PROTODIR=/c\\" > sedscript
	echo "PROTODIR=$$\{PROTODIR-$(IDIR)/gen\}" >> sedscript
	sed -f sedscript < genclass.sh > genclass
	chmod 0755 genclass
	rm -f sedscript

#to force sub-makes
FORCE:


###########################################################################
#
# Installation
#

MAKE_ENVIRON=\
	BINDIR=$(BINDIR) \
	LIBDIR=$(LIBDIR) \
	MANDIR=$(MANDIR) \
	INSTALL="$(INSTALL)"

all: src tests genclass etc gperf 

install:  install-lib install-include-files install-progs

install-lib:
	(cd src; $(MAKE) $(MAKE_ENVIRON) install)

install-progs:
	(cd etc; $(MAKE) $(MAKE_ENVIRON) install)
	(cd gperf; $(MAKE) $(MAKE_ENVIRON) install)
	$(INSTALL) genclass $(BINDIR)

install-include-files:
	-mkdir $(IDIR)
	-mkdir $(IDIR)/sys
	-mkdir $(IDIR)/gen
	cd $(SRCIDIR); \
	FILES=`find . ! -type d  -print`; \
	cd gen; \
	GFILES=`find . ! -type d -print`;\
	cd $(IDIR); \
	rm -fr $$FILES; \
	rm -f $$GFILES; \
	cd $(SRCIDIR); \
	FILES=`find . ! -type d  -print`; \
	for file in $$FILES; do \
		rm -f $(IDIR)/$$file; \
		cp $$file $(IDIR)/$$file; \
		chmod 0444 $(IDIR)/$$file; \
		echo $$file installed; \
	done


###########################################################################
#
# Destructors
#

clean:
	rm -f *.o *~ \#* *.bak *.pl a.out 
	cd tests; $(MAKE) clean
	cd etc; $(MAKE) clean
	cd gperf; $(MAKE) clean

realclean:
	cd src; $(MAKE) realclean
	cd tests; $(MAKE) realclean
	cd etc; $(MAKE) realclean
	cd gperf; $(MAKE) realclean
	-rm -f genclass
	-rm -f libg++.info* libg++.?? libg++.??s libg++.log libg++.toc libg++.*aux
	-rm -f *.orig src/*.orig tests/*.orig etc/*.orig g++-include/*.orig g++-include/sys/*.orig g++-include/gen/*.orig
	-rm -f *.rej src/*.rej tests/*.rej etc/*.rej g++-include/*.rej g++-include/sys/*.rej g++-include/gen/*.rej
	-rm -f *~ src/*~ tests/*~ etc/*~ g++-include/*~ g++-include/sys/*~ g++-include/gen/*~
	-rm -f a.out src/a.out tests/a.out etc/a.out 
	-rm -f *.s src/*.s tests/*.s etc/*.s 


.PHONY: src tests genclass etc gperf FORCE install install-include-files 
