# Makefile for gzip (GNU zip)    -*- Indented-Text -*-
# This is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License, see the file COPYING.

# Simple Atari-specific makefile for gcc.
# Written by Daniel Eriksson <den@hgs.se>
# Modified by Andreas Schwab <schwab@ls5.informatik.uni-dortmund.de>
# and Robert Fischer <fischer-robert@cs.yale.edu>.

# This Makefile is configured by default for the Atari ST using the
# Minix filesytem. It can be modified (for efficiency) for an Atari TT
# according to the instructions given below. It must be modified
# for building a TOS-filesystem version.

CC=gcc
BASIC_FLAGS = -O2 -DATARI

# Use this for a TT-only version
#TT_FLAGS = -m68020

# CFLAGS for building a Minix-filesystem version
CFLAGS = $(BASIC_FLAGS) $(TT_FLAGS)

# CFLAGS for building a TOS-filesystem version
#CFLAGS = $(BASIC_FLAGS) $(TT_FLAGS) -DTOSFS

AS=$(CC) -c
ASFLAGS = $(CFLAGS)
LDFLAGS =

OBJA = match.o
OBJS = bits.o crypt.o deflate.o getopt.o gzip.o inflate.o lzw.o \
  trees.o unlzw.o unpack.o unlzh.o unzip.o util.o zip.o $(OBJA)

gzip.ttp:	$(OBJS)
	$(CC) $(LDFLAGS) -o gzip.ttp $(OBJS)

gzip.o zip.o deflate.o trees.o bits.o unzip.o inflate.o: gzip.h tailor.h
util.o lzw.o unlzw.o unpack.o unlzh.o crypt.o: gzip.h tailor.h

gzip.o unlzw.o: revision.h lzw.h

bits.o unzip.o util.o zip.o: crypt.h

gzip.o getopt.o: getopt.h

match.o: match.S
	$(AS) $(ASLAGS) match.S
