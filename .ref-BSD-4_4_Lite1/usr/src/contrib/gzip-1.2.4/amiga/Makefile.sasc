# Makefile for gzip (GNU zip)    -*- Indented-Text -*-
# This Makefile is for SAS/C 6.3 on the Amiga
# Don't use it with SAS/C 6.2 or earlier versions. These versions will
# produce incorrect code for trees.c because of a bug in the optimizer.
# Made by Carsten Steger (carsten.steger@informatik.tu-muenchen.de)

# Some notes on gzip for the Amiga:
# Due to a strange implementation of the RAM-Disk on Commodore's part
# you should not use gzip -r on a directory in the RAM-Disk. To zip all
# files in a directory 'dir' in the RAM-Disk use something like
# gzip ram:dir/#? ram:dir/#?/#? ...
# where the number of '#?'s reflects the depth of the directory 'dir'.
#
# Alas, the program has it's full functionality only if you have
# Kickstart 2.0 or higher installed. This affects the expansion of
# wildcards and the preservation of file access times. You should use
# a shell that does wildcard expansion under Kickstart 1.3.


CC = sc

# If you have a 68020 or more you can define UNALIGNED_OK below
DEFS = DEF=AMIGA
LIBS = LIB LIB:scnb.lib

CFLAGS = DATA=FAR NOSTKCHK PARM=R IDIR=
OPTFLAGS = OPT OPTGO OPTPEEP OPTCOMP=3 OPTDEP=3 OPTRDEP=3
LDFLAGS = FROM LIB:c.o

OBJS = gzip.o zip.o deflate.o trees.o bits.o unzip.o inflate.o util.o \
      crypt.o lzw.o unlzw.o unpack.o unlzh.o getopt.o tailor.o match.o

.c.o:
	$(CC) $(DEFS) $(CFLAGS) $(OPTFLAGS) $<
.a.o:
	$(CC) $(DEFS) $(CFLAGS) $<

all: gzip

gzip: $(OBJS)
	slink $(LDFLAGS) $(OBJS) TO $@ $(LIBS)

gzip.o: gzip.c gzip.h tailor.h revision.h lzw.h getopt.h
zip.o: zip.c gzip.h tailor.h crypt.h
deflate.o: deflate.c gzip.h tailor.h lzw.h
trees.o: trees.c gzip.h tailor.h
bits.o: bits.c gzip.h tailor.h crypt.h
unzip.o: unzip.c gzip.h tailor.h crypt.h
inflate.o: inflate.c gzip.h tailor.h
util.o: util.c gzip.h tailor.h crypt.h
lzw.o: lzw.c lzw.h gzip.h tailor.h
unlzw.o: unlzw.c gzip.h tailor.h lzw.h
unpack.o: unpack.c gzip.h tailor.h crypt.h
unlzh.o: unlzh.c gzip.h tailor.h lzw.h
crypt.o: crypt.c
getopt.o: getopt.c getopt.h
tailor.o: tailor.c

match.o: match.a
