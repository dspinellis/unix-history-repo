# Makefile for GNU C++ compiler.
#   Copyright (C) 1987, 1988 Free Software Foundation, Inc.
#   Hacked by Michael Tiemann (tiemann@mcc.com)

#This file is part of GNU CC.

#GNU CC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 1, or (at your option)
#any later version.

#GNU CC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GNU CC; see the file COPYING.  If not, write to
#the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


# Variables that exist for you to override.
# See below for how to change them for certain systems.

# These files depend on -DSOS:
# cplus-class.o cplus-decl.o cplus-decl2.o cplus-init.o \
# cplus-method.o cplus-typeck.o

# This file depends on -DESKIT: cplus-class.o

# These files depend on -DGATHER_STATISTICS: tree.c cplus-lex.c \
# cplus-class.c cplus-search.c

# This file depends on -DCADILLAC
# cplus-cadillac.c
CADILLAC_OBJS = cplus-cadillac.o ../cadillac/code/connection/libconn.a

# Defining NDEBUG will cause assertion macros to do nothing.

# This file depends on -DYYDEBUG: cplus-tab.c.  Without, you cannot
# use `-dy'.

# On COFF systems, use one of the definitions below. (see collect.c)
# COFFLAGS = -DUSE_COLLECT -DCOFF
# COFFLAGS = -DUSE_COLLECT -DEXTENDED_COFF

# On some BSD systems (like Vax, unlike Sun), there is no file
# /usr/include/unistd.h.  If this is the case for your system
# uncomment the following line.
# HAVE_UNISTD_H = -DNO_UNISTD_H

# CFLAGS = -g -DSOS -DESKIT -O
CFLAGS = -g -DGATHER_STATISTICS -O $(COFFLAGS) -B../gcc-test/ -DFIELD_XREF
CC = gcc
BISON = bison
BISONFLAGS = -v -d
AR = ar
SHELL = /bin/sh
# on sysV, define this as cp.
INSTALL = install -c
# on sysV, define this as ln.
LINK = ln -s

# Compiler to use for compiling gnulib.
# OLDCC should not be the GNU C compiler.
OLDCC = cc

# CFLAGS for use with OLDCC, for compiling gnulib.
# NOTE: -O does not work on some Unix systems!
# If you use it here, you are asking for trouble.
CCLIBFLAGS=

# USG_STDIO if for machines which use System V stdio.
# In general, if USG is #defined by your system, you should
# uncomment the following line.  But turncoats who claim officially
# to support System V, but really try to be Berkeley (and thus
# break with USG defined), may need to define this.
# USG_STDIO = -DUSG_STDIO

prefix=/tools/gnu

# Directory where sources are, from where we are.
srcdir = .
# Directory in which to put the executable for the command `g++'
bindir = $(prefix)/bin
# Directory in which to put the subprograms used by the compiler.
libdir = $(prefix)/lib
# Directory in which to put the crt0+.o, crt1+.o, and other such files.
startdir = $(prefix)/lib
# Directory in which to put man pages.
mandir = $(prefix)/man/man1
# Number to put in man-page filename.
manext = 1

# Additional system libraries to link with.
CLIB=

# Change this to a null string if obstacks are installed in the
# system library.
OBSTACK=obstack.o

# Directory to link to, when using the target `maketest'.
DIR = ../gcc
# this is the GNU CC build directory
TDIR = ../gcc-test

# End of variables for you to override.


# Variables you should change for certain systems.

# These are what you would need on HPUX:
# CFLAGS = -Wc,-Ns2000 -Wc,-Ne700 -Wc,-Np300 -Wc,-Nw1000
# If you are using the GNU assembler and linker on HPUX,
# add -I../hp-include to CFLAGS.
# -g is desirable in CFLAGS, but a compiler bug in HPUX version 5
# bites whenever tree.def, rtl.def or machmode.def is included
# (ie., on every source file).
# CCLIBFLAGS = -Wc,-Ns2000 -Wc,-Ne700
# For CCLIBFLAGS you might want to specify the switch that
# forces only 68000 instructions to be used.

# If you are making gcc for the first time, and if you are compiling it with
# a non-gcc compiler, and if your system doesn't have a working alloca() in any
# of the standard libraries (as is true for HP/UX or Genix),
# then get alloca.c from GNU Emacs and un-comment the following line:
# ALLOCA = alloca.o
# But don't do that if compiling using GCC.

# If your system has alloca() in /lib/libPW.a, un-comment the following line:
# CLIB= -lPW

# If your system's malloc() routine fails for any reason (as it does on
# certain versions of Genix), try getting the files
# malloc.c and getpagesize.h from GNU Emacs and un-comment the following line:
MALLOC = malloc.o

# If you are running GCC on an Apollo (SR10.x),
# go into a Berkeley environment and use this:
# CFLAGS = -g -A nansi -A cpu,3000 -A runtype,bsd4.3 -A systype,any -DSHORT_ENUM_BUG
# (Says vasta@apollo.com.)


# Dependency on obstack, alloca, malloc or whatever library facilities
# are not installed in the system libraries.
LIBDEPS= $(OBSTACK) $(ALLOCA) $(MALLOC)

# How to link with both our special library facilities
# and the system's installed libraries.
LIBS = $(OBSTACK) $(ALLOCA) $(MALLOC) $(CLIB)

# Specify the directories to be searched for header files.
# Both . and srcdir are used, in that order,
# so that tm.h and config.h will be found in the compilation
# subdirectory rather than in the source directory.
INCLUDES = -I. -I$(srcdir) -I$(srcdir)/config
SUBDIR_INCLUDES = -I.. -I../$(srcdir) -I../$(srcdir)/config

# Always use -I$(srcdir)/config when compiling.
.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(INCLUDES) $<

# Language-specific object files for C.
C_OBJS = c-parse.tab.o c-decl.o c-typeck.o c-convert.o

# Language-specific object files for C++.
CPLUS_OBJS = cplus-tab.o cplus-decl.o cplus-decl2.o cplus-typeck.o cplus-type2.o \
 cplus-tree.o cplus-lex.o cplus-class.o cplus-init.o cplus-except.o \
 cplus-method.o cplus-cvt.o cplus-search.o cplus-ptree.o cplus-expr.o \
 cplus-field.o

# Language-independent object files.
OBJS = toplev.o version.o tree.o print-tree.o stor-layout.o \
 fold-const.o rtl.o rtlanal.o expr.o stmt.o case.o expmed.o explow.o \
 optabs.o varasm.o symout.o dbxout.o sdbout.o emit-rtl.o insn-emit.o \
 integrate.o jump.o cse.o loop.o flow.o stupid.o combine.o \
 regclass.o local-alloc.o global-alloc.o reload.o reload1.o caller-save.o \
 insn-peep.o final.o recog.o insn-recog.o insn-extract.o insn-output.o

# Files to be copied away after each stage in building.
STAGE_GCC=gcc
STAGESTUFF = *.o insn-flags.h insn-config.h insn-codes.h \
 insn-output.c insn-recog.c insn-emit.c insn-extract.c insn-peep.c \
 stamp-flags.h stamp-config.h stamp-codes.h \
 stamp-output.c stamp-recog.c stamp-emit.c stamp-extract.c stamp-peep.c \
 genemit genoutput genrecog genextract genflags gencodes genconfig genpeep \
 cc1plus

# Header files that are made available to programs compiled with gcc.
USER_H = stddef.h stdarg.h assert.h va-*.h limits.h

# The files that "belong" in CONFIG_H are deliberately omitted
# because having them there would not be useful in actual practice.
# All they would do is cause complete recompilation every time
# one of the machine description files is edited.
# That may or may not be what one wants to do.
# If it is, rm *.o is an easy way to do it.
# CONFIG_H = config.h tm.h
CONFIG_H =
RTL_H = rtl.h rtl.def machmode.def
TREE_H = tree.h real.h tree.def machmode.def
CPLUS_TREE_H = $(TREE_H) cplus-tree.h cplus-tree.def

# Note that dependencies on obstack.h are not written
# because that file is not part of GCC.
# Dependencies on gvarargs.h are not written
# because all that file does, when not compiling with GCC,
# is include the system varargs.h.

# gnulib is not a target because a proper installation of GNU CC
# will place it where g++ can find it.  Same with cpp
all: crt1+.o g++ cc1plus ld++ g++filt # collect crt0+.o
# On COFF systems, use the target below.
# all: g++ cc1plus collect # crt0+.o crt1+.o

doc: $(srcdir)/cpp.info $(srcdir)/g++.info

compilations: ${OBJS} ${CPLUS_OBJS}

g++: gcc.o version.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o g++ gcc.o version.o $(LIBS)

# Note: If you have SunOS 4.0, you can't use GNU LD to link programs
# which use shared libraries.  You could add -DNO_GNU_LD to this
# command line, but be warned that collect is not really powerful
# enough to do right by all C++ static constructors, and it would
# be better if you could just add support to GNU LD to handle
# shared libraries.
gcc.o: gcc.c $(CONFIG_H)
	$(CC) $(CFLAGS) $(INCLUDES) \
  -DSTANDARD_STARTFILE_PREFIX=\"$(libdir)/\" \
  -DSTANDARD_EXEC_PREFIX=\"$(libdir)/gcc-\" -c \
  `echo $(srcdir)/gcc.c | sed 's,^\./,,'`

version.o: version.c
obstack.o: obstack.c

ld.o: ld.c
	if cmp -s tm.h config/tm-sun3.h; then \
		OPTS='-Dmc68020 -DSUN3=3 -DTARGET=SUN3'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-sun3-fpa.h; then \
		OPTS='-Dmc68020 -DSUN3=3 -DTARGET=SUN3'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-sun3-nfp.h; then \
		OPTS='-Dmc68020 -DSUN3=3 -DTARGET=SUN3'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-sun2.h; then \
		OPTS='-Dmc68010 -DSUN2=2 -DTARGET=SUN2'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-sun4os3.h; then \
		OPTS='-DSUN4=4 -DTARGET=SUN4'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-sun4os4.h; then \
		OPTS='-DSUN4=4 -DTARGET=SUN4'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-sparc.h; then \
		OPTS='-DSUN4=4 -DTARGET=SUN4'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-encore.h; then \
		CRT0_OPTIONS='-DUMAX'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-hp9k320g.h; then \
		OPTS='-Dmc68020 -DNON_NATIVE -DUSG' \
		DFLTS='"$(libdir)/gnu", "/lib", "/usr/lib", "/usr/local/lib"'; \
	elif cmp -s tm.h config/tm-altos3068.h; then \
		OPTS='-DCOFF_ENCAPSULATE -DNON_NATIVE -DUSG -DPORTAR'; \
	elif cmp -s tm.h config/tm-newsgas.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES=bogus_fp, -DDOT_GLOBAL_START'; \
	elif cmp -s tm.h config/tm-i386gas.h; then \
		OPTS='-DUSG -DCOFF_ENCAPSULATE -DNON_NATIVE -DPORTAR'; \
	else \
		OPTS='-DTARGET=-1'; \
		DFLTS='"$(libdir)", "/lib", "/usr/lib", "/usr/local/lib"'; \
	fi; \
	echo $$OPTS $$DFLTS; \
	if [ "$$DFLTS" = "" ] ; \
	then \
	$(CC) -c -v $$OPTS $(PROFILE) $(CFLAGS) $(CFLAGS) ld.c ; \
	else \
	$(CC) -c -v $$OPTS -DSTANDARD_SEARCH_DIRS="$$DFLTS" $(PROFILE) $(CFLAGS) ld.c ; \
	fi

ld++: ld.o cplus-dem.o
	$(CC) -o ld++ $(PROFILE) ld.o cplus-dem.o -lg -lc

collect: collect.c config.h $(LIBDEPS)
	COLLECT_LIBS="-lld"; \
	if cmp -s tm.h config/tm-encore.h; then \
		COLLECT_OPTIONS='-DUMAX'; \
	else \
		COLLECT_OPTIONS=''; \
	fi; \
	$(CC) -o collect $(PROFILE) $$COLLECT_OPTIONS $(CFLAGS) $(INCLUDES) $< -lg -lc $$COLLECT_LIBS

collect2: collect2.c config.h
	$(CC) -o collect2 collect2.c $(PROFILE) $$COLLECT_OPTIONS $(CFLAGS) $(INCLUDES) -lc

g++filt: g++filt.o cplus-dem.o
	$(CC) -o g++filt g++filt.o cplus-dem.o

crt0+.o: crt0.c config.h
	-if cmp -s tm.h config/tm-vax.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES='; \
	elif cmp -s tm.h config/tm-vaxv.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES='; \
	elif cmp -s tm.h config/tm-vms.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES='; \
	elif cmp -s tm.h config/tm-hp9k320.h; then \
		CRT0_OPTIONS='-Umc68000'; \
	elif cmp -s tm.h config/tm-hp9k320g.h; then \
		CRT0_OPTIONS='-Umc68000'; \
	elif cmp -s tm.h config/tm-sequent.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES= -DDOT_GLOBAL_START'; \
	elif cmp -s tm.h config/tm-altos3068.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES= -Umc68000 -Um68k'; \
	elif cmp -s tm.h tm-i386gas.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES=bogus_fp, -DDOT_GLOBAL_START'; \
	elif cmp -s tm.h config/tm-hp9k3bsd.h; then \
		CRT0_OPTIONS='-DCRT0_DUMMIES=bogus_a6, -Uhp9000'; \
	else \
		CRT0_OPTIONS=''; \
	fi; \
	echo $$CRT0_OPTIONS; \
	$(CC) -Um68k $$CRT0_OPTIONS $(CFLAGS) -c $(INCLUDES) crt0.c
	mv crt0.o crt0+.o

CRT1_COMMAND=$(CC) -Um68k $(CFLAGS) -c $(INCLUDES) crt1.c

crt1+.o: crt1.c config.h
	$(CRT1_COMMAND)
	mv crt1.o crt1+.o

cc1plus: $(CPLUS_OBJS) $(OBJS) $(LIBDEPS) lastfile.o cplus-edsel.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o cc1plus lastfile.o $(CPLUS_OBJS) cplus-edsel.o $(OBJS) $(LIBS) -lm

cadillac-cc1plus: $(CPLUS_OBJS) $(CADILLAC_OBJS) $(OBJS) $(LIBDEPS) lastfile.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o cadillac-cc1plus lastfile.o $(CPLUS_OBJS) $(CADILLAC_OBJS) $(OBJS) $(LIBS)

cplus-decl.o : cplus-decl.c $(CONFIG_H) $(CPLUS_TREE_H) flags.h \
  cplus-parse.h cplus-decl.h stack.h rtl.h insn-flags.h
cplus-decl2.o : cplus-decl2.c $(CONFIG_H) $(CPLUS_TREE_H) flags.h \
  cplus-parse.h cplus-decl.h
cplus-typeck.o : cplus-typeck.c $(CONFIG_H) $(CPLUS_TREE_H) flags.h
cplus-type2.o : cplus-type2.c $(CONFIG_H) $(CPLUS_TREE_H) flags.h
cplus-tree.o : cplus-tree.c $(CONFIG_H) $(CPLUS_TREE_H)
cplus-class.o : cplus-class.c $(CONFIG_H) $(CPLUS_TREE_H) stack.h
cplus-init.o : cplus-init.c $(CONFIG_H) $(CPLUS_TREE_H)
cplus-except.o : cplus-except.c $(CONFIG_H) $(CPLUS_TREE_H)
cplus-expr.o : cplus-expr.c $(CONFIG_H) $(CPLUS_TREE_H) $(RTL_H) insn-codes.h
cplus-method.o : cplus-method.c $(CONFIG_H) $(CPLUS_TREE_H)
cplus-cvt.o : cplus-cvt.c $(CONFIG_H) $(CPLUS_TREE_H)
cplus-search.o : cplus-search.c $(CONFIG_H) $(CPLUS_TREE_H) stack.h
cplus-ptree.o : cplus-ptree.c $(CONFIG_H) $(TREE_H)
cplus-field.o : cplus-field.c $(CONFIG_H) $(TREE_H)

cplus-cadillac.o : cplus-cadillac.c $(CONFIG_H) $(CPLUS_TREE_H) stack.h
	$(CC) -c -DCADILLAC $(CFLAGS) $(INCLUDES) -I../cadillac/code/comp-dep -I../cadillac/code/connection $<
cplus-edsel.o : cplus-cadillac.c $(CONFIG_H) $(CPLUS_TREE_H) stack.h
	$(CC) -c $(CFLAGS) $(INCLUDES) -o cplus-edsel.o cplus-cadillac.c

cplus-tab.o : $(srcdir)/cplus-tab.c $(CONFIG_H) $(CPLUS_TREE_H) \
   cplus-parse.h flags.h input.h
	$(CC) -c $(CFLAGS) $(INCLUDES) -DPARSE_OUTPUT=\"$(PWD)/cplus-tab.out\" $<

$(srcdir)/cplus-tab.c : $(srcdir)/cplus-parse.y
	@echo expect 24 shift/reduce conflicts and 12 reduce/reduce conflicts
	cd $(srcdir) ; \
		$(BISON) $(BISONFLAGS) -o cplus-tab.c cplus-parse.y

cplus-lex.o : cplus-lex.c $(CONFIG_H) $(CPLUS_TREE_H) $(srcdir)/cplus-tab.h input.h
	$(CC) -c $(CFLAGS) $(INCLUDES) $(USG_STDIO) $<

lastfile.o : lastfile.c

# Language-independent files.

tree.o : tree.c $(CONFIG_H) $(TREE_H) flags.h
print-tree.o : print-tree.c $(CONFIG_H) $(TREE_H)
stor-layout.o : stor-layout.c $(CONFIG_H) $(TREE_H) $(RTL_H)
fold-const.o : fold-const.c $(CONFIG_H) $(TREE_H)
toplev.o : toplev.c $(CONFIG_H) $(TREE_H) flags.h input.h
	$(CC) -c $(CFLAGS) $(INCLUDES) $(HAVE_UNISTD_H) $<

rtl.o : rtl.c $(CONFIG_H) $(RTL_H)

rtlanal.o : rtlanal.c $(CONFIG_H) $(RTL_H)

varasm.o : varasm.c $(CONFIG_H) $(TREE_H) $(RTL_H) flags.h expr.h \
   insn-codes.h hard-reg-set.h
case.o : case.c $(CONFIG_H) $(RTL_H) $(TREE_H) insn-flags.h
stmt.o : stmt.c $(CONFIG_H) $(RTL_H) $(TREE_H) flags.h  \
   insn-flags.h expr.h insn-config.h regs.h hard-reg-set.h insn-codes.h
expr.o : expr.c $(CONFIG_H) $(RTL_H) $(TREE_H) flags.h  \
   insn-flags.h insn-codes.h expr.h insn-config.h recog.h
expmed.o : expmed.c $(CONFIG_H) $(RTL_H) $(TREE_H) flags.h  \
   insn-flags.h insn-codes.h expr.h insn-config.h recog.h
explow.o : explow.c $(CONFIG_H) $(RTL_H) $(TREE_H) flags.h expr.h insn-codes.h
optabs.o : optabs.c $(CONFIG_H) $(RTL_H) $(TREE_H) flags.h  \
   insn-flags.h insn-codes.h expr.h insn-config.h recog.h
symout.o : symout.c $(CONFIG_H) $(TREE_H) $(RTL_H) symseg.h gdbfiles.h
dbxout.o : dbxout.c $(CONFIG_H) $(TREE_H) $(RTL_H) flags.h $(CPLUS_TREE_H)
sdbout.o : sdbout.c $(CONFIG_H) $(TREE_H) $(RTL_H)

emit-rtl.o : emit-rtl.c $(CONFIG_H) $(RTL_H) regs.h insn-config.h real.h

integrate.o : integrate.c $(CONFIG_H) $(RTL_H) $(TREE_H) flags.h expr.h \
   insn-flags.h insn-codes.h

jump.o : jump.c $(CONFIG_H) $(RTL_H) flags.h regs.h
stupid.o : stupid.c $(CONFIG_H) $(RTL_H) regs.h hard-reg-set.h

cse.o : cse.c $(CONFIG_H) $(RTL_H) regs.h hard-reg-set.h flags.h real.h
loop.o : loop.c $(CONFIG_H) $(RTL_H) insn-config.h insn-codes.h \
   regs.h hard-reg-set.h recog.h flags.h expr.h
flow.o : flow.c $(CONFIG_H) $(RTL_H) basic-block.h regs.h hard-reg-set.h
combine.o : combine.c $(CONFIG_H) $(RTL_H) flags.h  \
   insn-config.h regs.h basic-block.h recog.h
regclass.o : regclass.c $(CONFIG_H) $(RTL_H) hard-reg-set.h flags.h \
   basic-block.h regs.h insn-config.h recog.h 
local-alloc.o : local-alloc.c $(CONFIG_H) $(RTL_H) flags.h basic-block.h regs.h \
   hard-reg-set.h insn-config.h recog.h
global-alloc.o : global-alloc.c $(CONFIG_H) $(RTL_H) flags.h  \
   basic-block.h regs.h hard-reg-set.h insn-config.h

reload.o : reload.c $(CONFIG_H) $(RTL_H) flags.h \
   reload.h recog.h hard-reg-set.h insn-config.h regs.h
reload1.o : reload1.c $(CONFIG_H) $(RTL_H) flags.h  \
   reload.h regs.h hard-reg-set.h insn-config.h basic-block.h recog.h
caller-save.o : caller-save.c $(CONFIG_H) $(RTL_H) flags.h \
   reload.h regs.h hard-reg-set.h insn-config.h basic-block.h recog.h
final.o : final.c $(CONFIG_H) $(RTL_H) flags.h regs.h recog.h conditions.h \
   gdbfiles.h insn-config.h real.h output.h
recog.o : recog.c $(CONFIG_H) $(RTL_H)  \
   regs.h recog.h hard-reg-set.h insn-config.h real.h

# Normally this target is not used; but it is used if you
# define ALLOCA=alloca.o.  In that case, you must get a suitable alloca.c
# from the GNU Emacs distribution.
# Note some machines won't allow $(CC) without -S on this source file.
alloca.o:	alloca.c
	$(CC) $(CFLAGS) -S `echo $(srcdir)/alloca.c | sed 's,^\./,,'`
	as alloca.s -o alloca.o

# Now the source files that are generated from the machine description.

.PRECIOUS: insn-config.h insn-flags.h insn-codes.h \
  insn-emit.c insn-recog.c insn-extract.c insn-output.c insn-peep.c

# The following pair of rules has this effect:
# genconfig is run only if the md has changed since genconfig was last run;
# but the file insn-config.h is touched only when its contents actually change.

# Each of the other insn-* files is handled by a similar pair of rules.

insn-config.h: stamp-config.h ;
stamp-config.h : md genconfig
	./genconfig md > tmp-insn-config.h
	$(srcdir)/move-if-change tmp-insn-config.h insn-config.h
	touch stamp-config.h

insn-flags.h: stamp-flags.h ;
stamp-flags.h : md genflags
	./genflags md > tmp-insn-flags.h
	$(srcdir)/move-if-change tmp-insn-flags.h insn-flags.h
	touch stamp-flags.h

insn-codes.h: stamp-codes.h ;
stamp-codes.h : md gencodes
	./gencodes md > tmp-insn-codes.h
	$(srcdir)/move-if-change tmp-insn-codes.h insn-codes.h
	touch stamp-codes.h

insn-emit.o : insn-emit.c $(CONFIG_H) $(RTL_H) expr.h real.h insn-codes.h \
  insn-config.h insn-flags.h
	$(CC) $(CFLAGS) $(INCLUDES) -c insn-emit.c

insn-emit.c: stamp-emit.c ;
stamp-emit.c : md genemit
	./genemit md > tmp-insn-emit.c
	$(srcdir)/move-if-change tmp-insn-emit.c insn-emit.c
	touch stamp-emit.c

insn-recog.o : insn-recog.c $(CONFIG_H) $(RTL_H) insn-config.h real.h
	$(CC) $(CFLAGS) $(INCLUDES) -c insn-recog.c

insn-recog.c: stamp-recog.c ;
stamp-recog.c : md genrecog
	./genrecog md > tmp-insn-recog.c
	$(srcdir)/move-if-change tmp-insn-recog.c insn-recog.c
	touch stamp-recog.c

insn-extract.o : insn-extract.c $(RTL_H)
	$(CC) $(CFLAGS) $(INCLUDES) -c insn-extract.c

insn-extract.c: stamp-extract.c ;
stamp-extract.c : md genextract
	./genextract md > tmp-insn-extract.c
	$(srcdir)/move-if-change tmp-insn-extract.c insn-extract.c
	touch stamp-extract.c

insn-peep.o : insn-peep.c $(CONFIG_H) $(RTL_H) regs.h real.h
	$(CC) $(CFLAGS) $(INCLUDES) -c insn-peep.c

insn-peep.c: stamp-peep.c ;
stamp-peep.c : md genpeep
	./genpeep md > tmp-insn-peep.c
	$(srcdir)/move-if-change tmp-insn-peep.c insn-peep.c
	touch stamp-peep.c

insn-output.o : insn-output.c $(CONFIG_H) $(RTL_H) regs.h real.h conditions.h \
    hard-reg-set.h insn-config.h insn-flags.h output.h aux-output.c
	$(CC) $(CFLAGS) $(INCLUDES) -c insn-output.c

insn-output.c: stamp-output.c ;
stamp-output.c : md genoutput
	./genoutput md > tmp-insn-output.c
	$(srcdir)/move-if-change tmp-insn-output.c insn-output.c
	touch stamp-output.c

# Now the programs that generate those files.
# $(CONFIG_H) is omitted from the deps of the gen*.o
# because these programs don't really depend on anything 
# about the target machine.  They do depend on config.h itself,
# since that describes the host machine.

genconfig : genconfig.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o genconfig genconfig.o rtl.o $(LIBS)

genconfig.o : genconfig.c $(RTL_H) config.h

genflags : genflags.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o genflags genflags.o rtl.o $(LIBS)

genflags.o : genflags.c $(RTL_H) config.h

gencodes : gencodes.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o gencodes gencodes.o rtl.o $(LIBS)

gencodes.o : gencodes.c $(RTL_H) config.h

genemit : genemit.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o genemit genemit.o rtl.o $(LIBS)

genemit.o : genemit.c $(RTL_H) config.h

genrecog : genrecog.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o genrecog genrecog.o rtl.o $(LIBS)

genrecog.o : genrecog.c $(RTL_H) config.h

genextract : genextract.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o genextract genextract.o rtl.o $(LIBS)

genextract.o : genextract.c $(RTL_H) config.h

genpeep : genpeep.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o genpeep genpeep.o rtl.o $(LIBS)

genpeep.o : genpeep.c $(RTL_H) config.h

genoutput : genoutput.o rtl.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o genoutput genoutput.o rtl.o $(LIBS)

genoutput.o : genoutput.c $(RTL_H) config.h

# Making the preprocessor
cpp: cccp
	-rm -f cpp
	ln cccp cpp
cccp: cccp.o cexp.o version.o $(LIBDEPS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o cccp cccp.o cexp.o version.o $(LIBS)
cexp.o: cexp.c
cexp.c: cexp.y
	$(BISON) $<
	mv cexp.tab.c cexp.c
cccp.o: cccp.c
	$(CC) $(CFLAGS) $(INCLUDES) \
          -DGCC_INCLUDE_DIR=\"$(libdir)/gcc-include\" \
          -DGPLUSPLUS_INCLUDE_DIR=\"$(libdir)/g++-include\" -c $<

$(srcdir)/g++.info: $(srcdir)/g++.texinfo
	makeinfo `echo $(srcdir)/g++.texinfo | sed 's,^\./,,'`

# gnulib is not deleted because deleting it would be inconvenient
# for most uses of this target.
clean:
	-rm -f $(STAGESTUFF) $(STAGE_GCC)
	-rm -f stamp-*.[ch] tmp-insn-*
	-rm -f *.s *.s[0-9] *.co *.greg *.lreg *.combine *.flow *.cse *.jump *.rtl *.tree *.loop *.dbr *.jump2
	-rm -f core hard-params

# Like clean but also delete the links made to configure gcc.
# Also removes gnulib, since that is desirable if you are changing cpus.
cleanconfig: clean
	-rm -f tm.h aux-output.c config.h md config.status gnulib

# Get rid of every file that's generated from some other file (except INSTALL).
realclean: cleanconfig
	-rm -f cpp.aux cpp.cps cpp.fns cpp.info cpp.kys cpp.pgs cpp.tps cpp.vrs
	-rm -f cplus-tab.c cplus-tab.out cplus-tab.h
	-rm -f errs gnulib cexp.c TAGS tm-*+.h
	-rm -f g++.info* g++.?? g++.??s g++.log g++.toc g++.*aux
	-rm -f *.dvi

# Copy the files into directories where they will be run.
install: all
	-mkdir $(libdir)
	-mkdir $(bindir)
	-mkdir $(startdir)
	-if [ -f cc1plus ] ; then $(INSTALL) cc1plus  $(libdir)/gcc-cc1plus ;fi
	-if [ -f collect ] ; then $(INSTALL) collect  $(libdir)/gcc-collect ;fi
	-if [ -f collect2 ] ; then $(INSTALL) collect2  $(libdir)/gcc-ld ;fi
	$(INSTALL) g++ $(bindir)
#	-if [ -f crt0+.o ] ; then $(INSTALL) crt0+.o $(startdir)/crt0+.o ;fi
	-if [ -f crt1+.o ] ; then $(INSTALL) crt1+.o $(startdir)/crt1+.o ;fi
	-mkdir $(libdir)/g++-include
	-chmod ugo+rx $(libdir)/g++-include

install-ld: ld++
	$(INSTALL) ld++ $(libdir)/gcc-ld

# do make -f Makefile maketest DIR=../gcc TDIR=../gcc-test
# in the intended test directory to make it a suitable test directory.
maketest:
	-rm -f =*
	-$(LINK) $(DIR)/.gdbinit .
	-$(LINK) $(DIR)/bison.simple .
	-$(LINK) $(DIR)/config.gcc .
	-$(LINK) $(DIR)/move-if-change .
	$(MAKE) clean
	./make-links.g++
	-$(LINK) $(DIR)/genemit.c $(DIR)/genoutput.c $(DIR)/genrecog.c \
      $(DIR)/genextract.c $(DIR)/genflags.c $(DIR)/gencodes.c \
      $(DIR)/genconfig.c $(DIR)/genpeep.c .
	-$(LINK) $(TDIR)/obstack.o $(TDIR)/rtl.o $(TDIR)/emit-rtl.o \
	  $(TDIR)/jump.o $(TDIR)/cse.o \
	  $(TDIR)/loop.o $(TDIR)/flow.o $(TDIR)/stupid.o $(TDIR)/combine.o \
	  $(TDIR)/regclass.o $(TDIR)/local-alloc.o $(TDIR)/global-alloc.o \
	  $(TDIR)/reload.o $(TDIR)/reload1.o $(TDIR)/caller-save.o \
	  $(TDIR)/final.o $(TDIR)/recog.o $(TDIR)/genemit \
	  $(TDIR)/genoutput $(TDIR)/genrecog $(TDIR)/genextract \
	  $(TDIR)/genflags $(TDIR)/gencodes $(TDIR)/genconfig \
	  $(TDIR)/genpeep .
	-$(LINK) $(TDIR)/genemit.o $(TDIR)/genoutput.o $(TDIR)/genrecog.o \
      $(TDIR)/genextract.o $(TDIR)/genflags.o $(TDIR)/gencodes.o \
      $(TDIR)/genconfig.o $(TDIR)/genpeep.o .
	-$(LINK) $(TDIR)/insn-emit.c $(TDIR)/insn-output.c $(TDIR)/insn-recog.c \
      $(TDIR)/insn-extract.c $(TDIR)/insn-flags.h $(TDIR)/insn-codes.h \
      $(TDIR)/insn-config.h $(TDIR)/insn-peep.c .
	-rm tm.h aux-output.c config.h md
# You must then run config.g++ to set up for compilation.

bootstrap: all force
	echo GNU C++ does not bootstrap itself

.PHONY: stage1 stage2 stage3 #In GNU Make, ignore whether `stage*' exists.
force:

TAGS: force
	-mkdir temp
	-mv cplus-tab.c cexp.c c-*.c *.y insn-*.[ch] gen*.c temp
	etags *.h *.c
	mv temp/* .
	rmdir temp

#In GNU Make, ignore whether `stage*' exists.
.PHONY: stage1 stage2 stage3 clean realclean TAGS bootstrap

force:
