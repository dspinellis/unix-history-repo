# Makefile for GNU Awk - sample ST version.
# This makefile for ST version of gcc compiler and associated libraries.
#
# This is a subset of the full Makefile cut down for Atari ST
# gcc compiler is assumed
# It is known to work with gulam shell.
# It may need some customization depending on your setup!
# Replace with it 'Makefile' from the source directory.
#
# You need sed.ttp for an automatic creation of config.h file!
# Check gulam script mkconf.g in atari directory.
# In a pinch you may create one by checking config/atari file and
# editing config.in by hand.
#
# Copyright (C) 1986, 1988-1992 the Free Software Foundation, Inc.
#
# This file is part of GAWK, the GNU implementation of the
# AWK Progamming Language.
#
# GAWK is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# GAWK is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

# cross-compiler
CC=              cgcc
# native
CC=              gcc
# comment out the following two lines if you do not want use
# 16-bit wide ints
WIDTH = -mshort
EXT=16

# for gcc 1.40
OPTIMIZE=       -O -fstrength-reduce -fcombine-regs -fomit-frame-pointer
# for gcc 2.2.2
#OPTIMIZE=      -O2 -fstrength-reduce -fomit-frame-pointer
PROFILE=        #-pg
DEBUG=          #-DMALLOCDEBUG #-DDEBUG #-DFUNC_TRACE #-DMPROF
LINKSTATIC=     #-Bstatic
WARN=           #-W -Wunused -Wimplicit -Wreturn-type -Wcomment # for gcc only

# Parser to use on grammar - any one of the following will work
#PARSER = yacc
PARSER = byacc
#PARSER = bison -y
# basename of parser output - adjust to your parser requirements
POUTPUT = awk_tab

# Set LIBS to any libraries that are machine specific
LIBS = -lpml$(EXT)

FLAGS=-G -Xlinker -x $(WIDTH)

CFLAGS= -DGAWK $(FLAGS) $(DEBUG) $(LINKSTATIC) $(PROFILE) $(OPTIMIZE) $(WARN)

# object files
AWKOBJS = main.o eval.o builtin.o msg.o iop.o io.o field.o array.o \
	node.o version.o missing.o re.o # getopt.o

ALLOBJS = $(AWKOBJS) awktab.o

# GNUOBJS
#       GNU stuff that gawk uses as library routines.
GNUOBJS= regex.o dfa.o $(ALLOCA)

# source and documentation files
SRC =   main.c eval.c builtin.c msg.c version.c \
	iop.c io.c field.c array.c node.c missing.c re.c getopt.c

ALLSRC= $(SRC) awktab.c

AWKSRC= awk.h awk.y $(ALLSRC) patchlevel.h protos.h config.in getopt.h

GNUSRC = alloca.c alloca.s dfa.c dfa.h regex.c regex.h

COPIES = missing/system.c missing/tzset.c \
	missing/memcmp.c missing/memcpy.c missing/memset.c \
	missing/random.c missing/strcase.c missing/strchr.c \
	missing/strerror.c missing/strtod.c \
	missing/strftime.c missing/strftime.3

SUPPORT = support/texindex.c support/texinfo.tex

DOCS= gawk.1 gawk.texi

INFOFILES= gawk.info gawk.info-* \
	gawk.aux gawk.cp gawk.cps gawk.fn gawk.fns gawk.ky gawk.kys \
	gawk.pg gawk.pgs gawk.toc gawk.tp gawk.tps gawk.vr gawk.vrs

MISC =  NEWS COPYING FUTURES Makefile.* PROBLEMS README* PORTS POSIX \
	mungeconf configure ACKNOWLEDGMENT LIMITATIONS

OTHERS= pc/* atari/* vms/*

ALLDOC= gawk.dvi $(INFOFILES)

ALLFILES= $(AWKSRC) $(GNUSRC) $(COPIES) $(MISC) $(DOCS) $(ALLDOC) $(OTHERS) \
	$(SUPPORT)

# set this for a version of toglclr you are using
TOGLFLAGS = -fload

# rules to build gawk
gawk.ttp: $(ALLOBJS) $(GNUOBJS) $(REOBJS)
	$(CC) -o $@ $(CFLAGS) $(ALLOBJS) $(GNUOBJS) $(REOBJS) $(LIBS)
	toglclr $(TOGLFLAGS) $@

$(AWKOBJS) regex.o dfa.o:	awk.h dfa.h regex.h

getopt.o:	getopt.h

main.o: patchlevel.h

awktab.c: awk.y
	$(PARSER) -v awk.y
	sed '/^extern char .malloc(), .realloc();$$/d' $(POUTPUT).c >awktab.c
	rm $(POUTPUT).c

config.h: config.in
	@echo You must provide a config.h!
	@echo Run \"./configure\" to build it for known systems
	@echo or copy config.in to config.h and edit it.; exit 1

gawk.dvi: gawk.texi
	tex gawk.texi; texindex gawk.??
	tex gawk.texi

gawk.info:	gawk.texi
	makeinfo gawk.texi

clean:
	rm *.o *.orig *.rej */*.orig */*.rej

cleaner:	clean
	rm gawk.ttp awktab.c

clobber: clean
	rm $(ALLDOC) gawk.log
