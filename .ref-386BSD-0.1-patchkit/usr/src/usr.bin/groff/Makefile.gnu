#Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.
#     Written by James Clark (jjc@jclark.uucp)
#
#This file is part of groff.
#
#groff is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free
#Software Foundation; either version 1, or (at your option) any later
#version.
#
#groff is distributed in the hope that it will be useful, but WITHOUT ANY
#WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#for more details.
#
#You should have received a copy of the GNU General Public License along
#with groff; see the file LICENSE.  If not, write to the Free Software
#Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# Define PAGE to be letter if your PostScript printer uses 8.5x11 paper (USA)
# and define it to be A4, if it uses A4 paper (rest of the world).
PAGE=A4
#PAGE=letter

# BINDIR says where to install executables.
BINDIR=/usr/local/bin

GROFFLIBDIR=/usr/local/lib/groff

# FONTDIR says where to install dev*/*.
FONTDIR=$(GROFFLIBDIR)/font

# FONTPATH says where to look for dev*/*.
FONTPATH=.:$(FONTDIR):/usr/local/lib/font:/usr/lib/font

# MACRODIR says where to install macros.
MACRODIR=$(GROFFLIBDIR)/tmac

# MACROPATH says where to look for macro files.
MACROPATH=.:$(MACRODIR):/usr/lib/tmac

# DEVICE is the default device.
DEVICE=ps

# PSPRINT is the command to use for printing a PostScript file.
# It must be a simple command, not a pipeline.
PSPRINT=lpr

# DVIPRINT is the command to use for printing a TeX dvi file.
# It must be a simple command, not a pipeline.
DVIPRINT=lpr -d

# HYPHENFILE is the file containing the hyphenation patterns.
HYPHENFILE=$(GROFFLIBDIR)/hyphen

# MANROOT is the root of the man page directory tree.
MANROOT=/usr/local/man

# MAN1EXT is the man section for user commands.
MAN1EXT=1
MAN1DIR=$(MANROOT)/man$(MAN1EXT)

# MAN5EXT is the man section for file formats.
MAN5EXT=5
MAN5DIR=$(MANROOT)/man$(MAN5EXT)

# MAN7EXT is the man section for macros.
MAN7EXT=7
MAN7DIR=$(MANROOT)/man$(MAN7EXT)

# The groff ms macros will be available as -m$(TMAC_S).
# If you use `TMAC_S=s', you can use the Unix ms macros by using
# groff -ms -M/usr/lib/tmac.
TMAC_S=gs

# Include -DBROKEN_SPOOLER in CPPDEFINES if you have a PostScript
# spooler or previewer that is unable to cope with anything after
# %%EndProlog and before the first page (old versions of TranScript
# have this problem) or gets confused by included documents (the Sun
# pageview previewer has this problem), or if you are going to be
# sharing PostScript documents with others who might be running such a
# spooler or previewer.
# Include -DCFRONT_ANSI_BUG if you are using AT&T C++ 2.0 with an ANSI C
# compiler backend.
# Include -DHAVE_VFORK if you have vfork().
# Include -DHAVE_SYS_SIGLIST if you have sys_siglist[].
# Include -DHAVE_UNION_WAIT if wait() is declared by osfcn.h to take
# an argument of type union wait * (Sun C++ does this).  Don't include
# it if you're using the libg++ header files.
CPPDEFINES=-DBROKEN_SPOOLER -DHAVE_VFORK -DHAVE_SYS_SIGLIST # -DHAVE_UNION_WAIT -DCFRONT_ANSI_BUG

# Uncomment the next line if you don't have fmod in your math library.
# I believe this is needed on Ultrix and BSD 4.3.
# FMOD=fmod.o

# Uncomment the next line if you don't have strtol in your C library.
# I believe this is needed on BSD 4.3.
# STRTOL=strtol.o

# Additional flags needed to compile lib/malloc.c
# Use this with BSD.
MALLOCFLAGS=-DBSD
# Use this with System V
# MALLOCFLAGS=-DUSG
# Use this with SunOS 4.1
# MALLOCFLAGS=-DBSD -DSUNOS_LOCALTIME_BUG

# Comment this out if the GNU malloc gives you problems, or if you would
# prefer to use the system malloc.
MALLOC=malloc.o

GROFF=
# Comment the next line out if groff.c gives problems.
GROFF=groff

# CC is the C++ compiler
CC=g++
# I'm told that -fno-inline is needed on a 68030-based Apollo
# CC=g++ -fno-inline

# OLDCC is the C compiler.
OLDCC=gcc

PROFILE_FLAG=
DEBUG_FLAG=-g
OPTIMIZE_FLAG=-O
WARNING_FLAGS=#-Wall -Wcast-qual -Wwrite-strings

# Use this to pass additional flags on the command line.
XCFLAGS=

# CFLAGS are passed to sub makes
CFLAGS=$(PROFILE_FLAG) $(DEBUG_FLAG) $(OPTIMIZE_FLAG) $(WARNING_FLAGS) \
	$(CPPDEFINES) $(XCFLAGS)

XOLDCFLAGS=
# OLDCFLAGS are passed to sub makes
OLDCFLAGS=$(DEBUG_FLAG) $(PROFILE_FLAG) $(OPTIMIZE_FLAG) $(XOLDCFLAGS)

XLDFLAGS=
LDFLAGS=$(PROFILE_FLAG) $(DEBUG_FLAG) $(XLDFLAGS)
# Libraries needed for linking C++ programs.
LIBS=
# Libraries needed for linking C++ programs that use libm.a.
MLIBS=$(LIBS) -lm

AR=ar

# Define RANLIB to be empty if you don't have ranlib.
RANLIB=ranlib

# YACC can be either yacc or bison -y
YACC=bison -y
YACCFLAGS=-v

ETAGS=/usr/local/bin/etags
# Flag to make etags treat *.[ch] files as C++
ETAGSFLAGS=-p

SHELL=/bin/sh

SUBDIRS=lib troff pic tbl eqn etc driver ps tty dvi macros man

# SUBFLAGS says what flags to pass to sub makes
SUBFLAGS="CC=$(CC)" "CFLAGS=$(CFLAGS)" "LDFLAGS=$(LDFLAGS)" \
	"OLDCC=$(OLDCC)" "OLDCFLAGS=$(OLDCFLAGS)" \
	"YACC=$(YACC)" "YACCFLAGS=$(YACCFLAGS)" \
	"DEVICE=$(DEVICE)" "FONTPATH=$(FONTPATH)" "MACROPATH=$(MACROPATH)" \
	"MALLOCFLAGS=$(MALLOCFLAGS)" "MALLOC=$(MALLOC)" \
	"FMOD=$(FMOD)" "STRTOL=$(STRTOL)" \
	"AR=$(AR)" "RANLIB=$(RANLIB)" "LIBS=$(LIBS)" "MLIBS=$(MLIBS)" \
	"FONTDIR=$(FONTDIR)" "BINDIR=$(BINDIR)" "PAGE=$(PAGE)" \
	"MACRODIR=$(MACRODIR)" "HYPHENFILE=$(HYPHENFILE)" \
	"TMAC_S=$(TMAC_S)" "MAN1EXT=$(MAN1EXT)" "MAN1DIR=$(MAN1DIR)" \
	"MAN5EXT=$(MAN5EXT)" "MAN5DIR=$(MAN5DIR)" \
	"MAN7EXT=$(MAN7EXT)" "MAN7DIR=$(MAN7DIR)"

all: $(SUBDIRS) $(GROFF) shgroff

$(SUBDIRS): FORCE
	@cd $@; \
	echo Making all in $@; \
	$(MAKE) $(SUBFLAGS) all

troff pic tbl eqn etc ps tty dvi: lib
ps tty dvi: driver

TAGS: FORCE
	@for dir in $(SUBDIRS); do \
	echo Making TAGS in $$dir; \
	(cd $$dir; $(MAKE) "ETAGSFLAGS=$(ETAGSFLAGS)" "ETAGS=$(ETAGS)" TAGS); \
	done

topclean: FORCE
	-rm -f shgroff
	-rm -f groff *.o core

clean: topclean FORCE
	@for dir in $(SUBDIRS) doc; do \
	echo Making clean in $$dir; \
	(cd $$dir; $(MAKE) clean); done

distclean: topclean FORCE
	@for dir in $(SUBDIRS) doc; do \
	echo Making distclean in $$dir; \
	(cd $$dir; $(MAKE) distclean); done

# You really don't want to use this target.
realclean: topclean FORCE
	@for dir in $(SUBDIRS) doc; do \
	echo Making realclean in $$dir; \
	(cd $$dir; $(MAKE) realclean); done

install.nobin: FORCE shgroff
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	-[ -d $(GROFFLIBDIR) ] || mkdir $(GROFFLIBDIR)
	-[ -d $(MANROOT) ] || mkdir $(MANROOT)
	@for dir in $(SUBDIRS); do \
	echo Making install.nobin in $$dir; \
	(cd $$dir; $(MAKE) $(SUBFLAGS) install.nobin); done
	if [ -z "$(GROFF)" ] ; then cp shgroff $(BINDIR)/groff ; fi

install.bin: FORCE $(GROFF)
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	@for dir in $(SUBDIRS); do \
	echo Making install.bin in $$dir; \
	(cd $$dir; $(MAKE) $(SUBFLAGS) install.bin); done
	if [ -n "$(GROFF)" ] ; then cp groff $(BINDIR)/groff ; fi

install: install.bin install.nobin

install.mm: FORCE
	-[ -d $(GROFFLIBDIR) ] || mkdir $(GROFFLIBDIR)
	-[ -d $(MACRODIR) ] || mkdir $(MACRODIR)
	sed -f macros/fixmacros.sed -e 's;/usr/lib/tmac;$(MACRODIR);' \
	    /usr/lib/macros/mmt >$(MACRODIR)/tmac.m
	sed -f macros/fixmacros.sed /usr/lib/tmac/sys.name \
	    >$(MACRODIR)/sys.name
	patch -s $(MACRODIR)/tmac.m macros/mm.diff

shgroff: groff.sh
	@echo Making $@ from groff.sh
	@-rm -f $@
	@sed -e "s;@BINDIR@;$(BINDIR);" \
	-e "s;@DEVICE@;$(DEVICE);" \
	-e "s;@PROG_PREFIX@;$(PROG_PREFIX);" \
	-e "s;@FONTDIR@;$(FONTDIR);" \
	-e "s;@PSPRINT@;$(PSPRINT);" \
	-e "s;@DVIPRINT@;$(DVIPRINT);" \
	groff.sh >$@ || rm -f $@
	@chmod +x $@

groff: groff.o lib/libgroff.a
	$(CC) $(LDFLAGS) -o $@ groff.o lib/libgroff.a $(LIBS)

lib/libgroff.a: lib

groff.o: groff.c stringify
	$(CC) -c -Ilib $(CFLAGS) -DDEVICE=\"$(DEVICE)\" \
	"-DPSPRINT=`$(SHELL) stringify $(PSPRINT)`" \
	"-DDVIPRINT=`$(SHELL) stringify $(DVIPRINT)`" \
	groff.c

groff.o: lib/lib.h lib/errarg.h lib/error.h lib/stringclass.h lib/font.h

bindist: all VERSION Makefile.bd README.bd FORCE
	-[ -d bindist ] || mkdir bindist
	@topdir=`pwd`; \
	for dir in $(SUBDIRS); do \
	(cd $$dir; $(MAKE) "BINDIR=$$topdir/bindist" install.bin); done
	cp README.bd bindist/README
	cp VERSION bindist
	if [ "$(GROFF)" ] ; then cp groff bindist/groff ; fi
	@echo Making bindist/Makefile
	@sed -e "s;@GROFFLIBDIR@;$(GROFFLIBDIR);" \
	-e "s;@FONTDIR@;$(FONTDIR);" \
	-e "s;@FONTPATH@;$(FONTPATH);" \
	-e "s;@MACRODIR@;$(MACRODIR);" \
	-e "s;@MACROPATH@;$(MACROPATH);" \
	-e "s;@HYPHENFILE@;$(HYPHENFILE);" \
	-e "s;@DEVICE@;$(DEVICE);" \
	-e "s;@GROFF@;$(GROFF);" \
	Makefile.bd >bindist/Makefile

FORCE:
