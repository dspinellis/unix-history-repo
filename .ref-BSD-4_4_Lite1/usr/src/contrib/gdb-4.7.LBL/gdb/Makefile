# This file was generated automatically by configure.  Do not edit.
VPATH = .
links =  xm.h tm.h nm.h
host_alias = hp300bsd
host_cpu = m68k
host_vendor = hp
host_os = bsd
target_alias = hp300bsd
target_cpu = m68k
target_vendor = hp
target_os = bsd
target_makefile_frag = config/hp300bsd.mt
host_makefile_frag = config/hp300bsd.mh
ALL=all.internal
#Copyright 1989, 1990, 1991, 1992 Free Software Foundation, Inc.

# This file is part of GDB.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

prefix = /usr/local

program_transform_name = 
exec_prefix = $(prefix)
bindir = /usr/bin
libdir = $(exec_prefix)/lib
tooldir = $(libdir)/hp300bsd

datadir = $(prefix)/lib
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info
includedir = $(prefix)/include
docdir = $(datadir)/doc

SHELL = /bin/sh

xbindir = /usr/bin/X11
xlibdir = /usr/lib/X11

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = ar
AR_FLAGS = qv
RANLIB = ranlib

# Flags that describe where you can find the termcap library.
# This can be overridden in the host Makefile fragment file.
TERMCAP = -ltermcap

# System V: If you compile gdb with a compiler which uses the coff
# encapsulation feature (this is a function of the compiler used, NOT
# of the m-?.h file selected by config.gdb), you must make sure that
# the GNU nm is the one that is used by munch.

# If you are compiling with GCC, make sure that either 1) You use the
# -traditional flag, or 2) You have the fixed include files where GCC
# can reach them.  Otherwise the ioctl calls in inflow.c
# will be incorrectly compiled.  The "fixincludes" script in the gcc
# distribution will fix your include files up.
#CC=cc
#CC=gcc -traditional
CC=gcc -O2
GCC=gcc

# Directory containing source files.  Don't clean up the spacing,
# this exact string is matched for by the "configure" script.
srcdir = .

# It is also possible that you will need to add -I/usr/include/sys to the
# CFLAGS section if your system doesn't have fcntl.h in /usr/include (which 
# is where it should be according to Posix).

BISON=yacc
YACC=$(BISON)

# Documentation (gdb.dvi) needs either GNU m4 or SysV m4; 
# Berkeley/Sun don't have quite enough. 
#M4=/usr/5bin/m4
M4=gm4

# where to find texinfo; GDB dist should include a recent one
TEXIDIR=${srcdir}/../texinfo/fsf

# where to find makeinfo, preferably one designed for texinfo-2
MAKEINFO=makeinfo

# Set this up with gcc if you have gnu ld and the loader will print out
# line numbers for undefinded refs.
#CC-LD=gcc -static
CC-LD=${CC}

# Where is the "include" directory?  Traditionally ../include or ./include
INCLUDE_DIR =  ${srcdir}/../include
INCLUDE_DEP = $$(INCLUDE_DIR)

# Where is the source dir for the MMALLOC library? Traditionally ../mmalloc
# or ./mmalloc  (When we want the binary library built from it, we use
# ${MMALLOC_DIR}${subdir}.)
# Note that mmalloc can still be used on systems without mmap().
# To use your system malloc, comment out the following defines.
MMALLOC_DIR = ${srcdir}/../mmalloc
MMALLOC_DEP = $$(MMALLOC_DIR)
MMALLOC_LIB = ./../mmalloc${subdir}/libmmalloc.a
# To use your system malloc, uncomment MMALLOC_DISABLE.
#MMALLOC_DISABLE = -DNO_MMALLOC
# To use mmalloc but disable corruption checking, uncomment MMALLOC_CHECK
#MMALLOC_CHECK = -DNO_MMALLOC_CHECK
MMALLOC_CFLAGS = ${MMALLOC_CHECK} ${MMALLOC_DISABLE}

# Where is the source dir for the BFD library?  Traditionally ../bfd or ./bfd
# (When we want the binary library built from it, we use ${BFD_DIR}${subdir}.)
BFD_DIR =  ${srcdir}/../bfd
BFD_DEP = $$(BFD_DIR)
BFD_LIB = ./../bfd${subdir}/libbfd.a

# Where is the source dir for the READLINE library?  Traditionally in .. or .
# (For the binary library built from it, we use ${READLINE_DIR}${subdir}.)
READLINE_DIR = ${srcdir}/../readline
READLINE_DEP = $$(READLINE_DIR)
RL_LIB = ./../readline${subdir}/libreadline.a

# All the includes used for CFLAGS and for lint.
# -I. for config files.
# -I${srcdir} possibly for regex.h also.
INCLUDE_CFLAGS = -I. -I${srcdir} -I$(INCLUDE_DIR)

# M{H,T}_CFLAGS, if defined, has host- and target-dependent CFLAGS
# from the config/ directory.
GLOBAL_CFLAGS = ${MT_CFLAGS} ${MH_CFLAGS}
#PROFILE_CFLAGS = -pg

# CFLAGS is specifically reserved for setting from the command line
# when running make.  I.E.  "make USER_CFLAGS=-Wmissing-prototypes".
CFLAGS = -g
# INTERNAL_CFLAGS is the aggregate of all other *CFLAGS macros.
INTERNAL_CFLAGS = ${CFLAGS} ${GLOBAL_CFLAGS} ${PROFILE_CFLAGS} ${MMALLOC_CFLAGS} ${INCLUDE_CFLAGS} ${USER_CFLAGS}
# None of the things in CFLAGS will do any harm, and on some systems
#  (e.g. SunOS4) it is important to use the MH_CFLAGS.
LDFLAGS = $(CFLAGS)

# Where is the "-liberty" library, containing getopt and obstack?
LIBIBERTY_DIR = ${srcdir}/../libiberty
LIBIBERTY = ./../libiberty${subdir}/libiberty.a

# Where is the "-lopcodes" library, with (some of) the opcode tables and
# disassemblers?
OPCODES = ./../opcodes${subdir}/libopcodes.a

# The config/mh-* file must define REGEX and REGEX1 on USG machines.
# If your sysyem is missing alloca(), or, more likely, it's there but
# it doesn't work, define ALLOCA & ALLOCA1 too.

# Libraries and corresponding dependencies for compiling gdb.
# {X,T}M_CLIBS, defined in *config files, have host- and target-dependent libs.
# TERMCAP comes after readline, since readline depends on it.
CLIBS = ${BFD_LIB} ${RL_LIB} ${TERMCAP} ${OPCODES} ${MMALLOC_LIB} ${LIBIBERTY} \
	${XM_CLIBS} ${TM_CLIBS} ${NAT_CLIBS}
CDEPS = ${XM_CDEPS} ${TM_CDEPS} ${NAT_CDEPS} \
	${BFD_LIB} ${RL_LIB}            ${OPCODES} ${MMALLOC_LIB} ${LIBIBERTY}

ADD_FILES = ${REGEX} ${ALLOCA}  ${XM_ADD_FILES} ${TM_ADD_FILES} ${NAT_ADD_FILES}
ADD_DEPS = ${REGEX1} ${ALLOCA1} ${XM_ADD_FILES} ${TM_ADD_FILES} ${NAT_ADD_FILES}

VERSION = 4.7
DIST=gdb

LINT=/usr/5bin/lint
LINTFLAGS= -I${BFD_DIR}

# Host and target-dependent makefile fragments come in here.
####
# Target: Hewlett-Packard 9000 series 300, running BSD
TDEPFILES= exec.o m68k-pinsn.o m68k-tdep.o m68kbsd-tdep.o
TM_FILE= tm-hp300bsd.h
TM_CLIBS= -lkvm
MT_CFLAGS = -DKERNELDEBUG
# Host: Hewlett-Packard 9000 series 300, running BSD
XDEPFILES= infptrace.o inftarg.o fork-child.o coredep.o corelow.o
XM_FILE= xm-hp300bsd.h
REGEX= regex.o
REGEX1= regex.o
# End of host and target-dependent makefile fragments
LBL_SRC = kcore.c remote-sl.c remote-fp.c kernel.c cmdparse.c
LBL_OBJ = $(LBL_SRC:.c=.o)

#SYMREADERS = dbxread.c coffread.c elfread.c dwarfread.c \
#		xcoffread.c stabsread.c mipsread.c
SYMREADERS = dbxread.c stabsread.c
SYMREADOBJ = $(SYMREADERS:.c=.o)

FLAGS_TO_PASS = \
	"against=$(against)" \
	"AR=$(AR)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC=$(CC)" \
	"CFLAGS=$(CFLAGS) $(MT_CFLAGS)" \
	"RANLIB=$(RANLIB)" \
	"MAKEINFO=$(MAKEINFO)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"BISON=$(BISON)"

# Source files in the main directory.
# Files which are included via a config/* Makefile fragment
# should *not* be specified here; they're in "ALLDEPFILES".
SFILES_MAINDIR = \
	blockframe.c breakpoint.c command.c core.c demangle.c \
	environ.c eval.c expprint.c findvar.c infcmd.c inflow.c infrun.c \
	main.c printcmd.c gdbtypes.c \
	remote.c source.c stack.c symmisc.c symtab.c symfile.c \
	utils.c valarith.c valops.c valprint.c values.c c-exp.y m2-exp.y \
	mem-break.c target.c \
	$(SYMREADERS) \
	ieee-float.c language.c parse.c buildsym.c objfiles.c \
	minsyms.c maint.c \
	$(LBL_SRC) xgdb.c

# Source files in subdirectories (which will be handled separately by
#  'make gdb.tar.Z').
# Files which are included via a config/* Makefile fragment
# should *not* be specified here; they're in "ALLDEPFILES".
SFILES_SUBDIR = \
	 ${srcdir}/vx-share/dbgRpcLib.h \
	 ${srcdir}/vx-share/ptrace.h \
	 ${srcdir}/vx-share/reg.h \
	 ${srcdir}/vx-share/vxTypes.h \
	 ${srcdir}/vx-share/vxWorks.h \
	 ${srcdir}/vx-share/wait.h \
	 ${srcdir}/vx-share/xdr_ld.h \
	 ${srcdir}/vx-share/xdr_ptrace.h \
	 ${srcdir}/vx-share/xdr_rdb.h \
	 ${srcdir}/vx-share/xdr_regs.h \
	 ${srcdir}/nindy-share/b.out.h \
	 ${srcdir}/nindy-share/block_io.h \
	 ${srcdir}/nindy-share/coff.h \
	 ${srcdir}/nindy-share/demux.h \
	 ${srcdir}/nindy-share/env.h \
	 ${srcdir}/nindy-share/stop.h \
	 ${srcdir}/nindy-share/ttycntl.h

# Non-source files in subdirs, that should go into gdb.tar.Z.
NONSRC_SUBDIR = \
	 ${srcdir}/nindy-share/Makefile \
	 ${srcdir}/nindy-share/VERSION \
	 ${srcdir}/nindy-share/README \
	 ${srcdir}/vx-share/README

# All source files that go into linking GDB, except config-specified files.
SFILES = $(SFILES_MAINDIR) $(SFILES_SUBDIR)

# All source files that lint should look at
LINTFILES = $(SFILES) $(YYFILES) init.c

# Any additional files specified on these lines should also be added to
# the OTHERS = definition below, so they go in the tar files.
SFILES_STAND = $(SFILES) standalone.c
SFILES_KGDB  = $(SFILES) stuff.c kdb-start.c

# Header files that are not named in config/* Makefile fragments go here.
HFILES=	breakpoint.h buildsym.h call-cmds.h command.h defs.h \
	environ.h expression.h frame.h gdbcmd.h gdbcore.h gdbtypes.h \
	gdb-stabs.h ieee-float.h inferior.h language.h \
	minimon.h objfiles.h parser-defs.h partial-stab.h \
	serial.h signals.h solib.h symfile.h symtab.h stabsread.h \
	target.h terminal.h xcoffsolib.h value.h \
	tm-68k.h tm-hppa.h tm-i960.h tm-sparc.h tm-sunos.h tm-sysv4.h \
	xm-m68k.h xm-sparc.h xm-sysv4.h xm-vax.h \
	nm-i386bsd.h nm-i386mach.h nm-i386sco.h nm-i386v.h nm-i386v4.h nm-irix3.h \
	nm-irix4.h nm-linux.h nm-m88k.h nm-mips.h nm-news.h nm-rs6000.h \
	nm-sun2.h nm-sun3.h nm-sun386.h nm-sun4os4.h nm-trash.h \
	nm-ultra3.h nm-hppab.h nm-hppah.h nm-umax.h nm-sysv4.h

REMOTE_EXAMPLES = m68k-stub.c i386-stub.c sparc-stub.c rem-multi.shar

POSSLIBS_MAINDIR = regex.c regex.h alloca.c
POSSLIBS = $(POSSLIBS_MAINDIR)

TESTS = testbpt.c testfun.c testrec.c testreg.c testregs.c

OTHERS = Makefile.in depend alldeps.mak createtags munch configure.in \
	 ChangeLog ChangeLog-9091 ChangeLog-3.x gdb.1 refcard.ps \
	 README TODO TAGS NEWS Projects \
	 .gdbinit COPYING $(YYFILES) \
	 copying.c Convex.notes copying.awk \
	 saber.suppress standalone.c stuff.c kdb-start.c \
	 putenv.c gcc.patch XGDB-README

# Subdirectories of gdb, which should be included in their entirety in
# gdb-xxx.tar.Z:
TARDIRS = doc # tests

# GDB "info" files, which should be included in their entirety
INFOFILES = gdb.info*

DEPFILES= ${TDEPFILES} ${XDEPFILES} ${NATDEPFILES}

SOURCES=$(SFILES) $(ALLDEPFILES) $(YYFILES)
TAGFILES = $(SOURCES) ${HFILES} ${ALLPARAM} ${POSSLIBS} 
TAGFILES_MAINDIR = $(SFILES_MAINDIR) $(ALLDEPFILES_MAINDIR) \
             ${HFILES} ${ALLPARAM} ${POSSLIBS_MAINDIR} 
TARFILES = ${TAGFILES_MAINDIR} ${OTHERS} ${REMOTE_EXAMPLES}

OBS = version.o main.o blockframe.o breakpoint.o findvar.o stack.o source.o \
    values.o eval.o valops.o valarith.o valprint.o printcmd.o \
    symtab.o symfile.o symmisc.o infcmd.o infrun.o remote.o \
    command.o utils.o expprint.o environ.o gdbtypes.o \
    copying.o $(DEPFILES) mem-break.o target.o \
    ieee-float.o putenv.o parse.o language.o $(YYOBJ) \
    buildsym.o objfiles.o minsyms.o maint.o demangle.o \
    $(SYMREADOBJ) core.o \
    $(LBL_OBJ)

RAPP_OBS = rgdb.o rudp.o rserial.o serial.o udp.o $(XDEPFILES)

TSOBS = inflow.o

NTSOBS = standalone.o

TSSTART = /lib/crt0.o

NTSSTART = kdb-start.o

SUBDIRS = doc

# For now, shortcut the "configure GDB for fewer languages" stuff.
YYFILES = c-exp.tab.c m2-exp.tab.c
YYOBJ = c-exp.tab.o m2-exp.tab.o

# Prevent Sun make from putting in the machine type.  Setting
# TARGET_ARCH to nothing works for SunOS 3, 4.0, but not for 4.1.
.c.o:
	${CC} -c ${INTERNAL_CFLAGS} $<

all: gdb
	$(MAKE) subdir_do DO=all "DODIRS=$(SUBDIRS)" $(FLAGS_TO_PASS)
check:
info: force
	$(MAKE) subdir_do DO=info "DODIRS=$(SUBDIRS)" $(FLAGS_TO_PASS)
install-info: force
	$(MAKE) subdir_do DO=install-info "DODIRS=$(SUBDIRS)" $(FLAGS_TO_PASS)
clean-info: force
	$(MAKE) subdir_do DO=clean-info "DODIRS=$(SUBDIRS)" $(FLAGS_TO_PASS)

install.xgdb: xgdb
	rm -f $(xbindir)/xgdb
	install xgdb $(xbindir)
	install Xgdb.ad $(xlibdir)/app-defaults/Xgdb

gdb.z:gdb.1
	nroff -man $(srcdir)/gdb.1 | col -b > gdb.t 
	pack gdb.t ; rm -f gdb.t
	mv gdb.t.z gdb.z
	
install: gdb 
	install -s gdb /usr/contrib/bin

init.c: $(srcdir)/munch $(OBS) $(TSOBS)
	$(srcdir)/munch ${MUNCH_DEFINE} $(OBS) $(TSOBS) > init.c

xinit.c: $(srcdir)/munch $(OBS) $(TSOBS) xgdb.o
	rm -f xinit.c
	./munch ${MUNCH_DEFINE} $(OBS) $(TSOBS) xgdb.o > xinit.c

xgdb: $(OBS) $(TSOBS) xgdb.o xinit.o ${ADD_DEPS}
	$(CC-LD) $(LDFLAGS) -o xgdb xinit.o xgdb.o \
		$(OBS) $(TSOBS) $(ADD_FILES) $(CLIBS) $(LOADLIBES) \
		-lXaw -lXmu -lXt -lXext -lX11 -lXwchar -lX11

gdb: $(OBS) $(TSOBS) ${ADD_DEPS} ${CDEPS} init.o
	${CC-LD} $(GLOBAL_CFLAGS) $(LDFLAGS) -o gdb \
	  init.o $(OBS) $(TSOBS) $(ADD_FILES) $(CLIBS) $(LOADLIBES)

saber_gdb: $(SFILES) $(DEPFILES) copying.c version.c
	#setopt load_flags $(CFLAGS) -I$(BFD_DIR) -DHOST_SYS=SUN4_SYS
	#load ./init.c $(SFILES)
	#unload ${srcdir}/c-exp.y ${srcdir}/m2-exp.y ${srcdir}/vx-share/*.h
	#unload ${srcdir}/nindy-share/[A-Z]*
	#load c-exp.tab.c m2-exp.tab.c
	#load copying.c version.c
	#load ../opcodes/libopcodes.a
	#load ../libiberty/libiberty.a
	#load ../bfd/libbfd.a
	#load ../readline/libreadline.a
	#load ../mmalloc/libmmalloc.a
	#load -ltermcap 
	#load `echo " "$(DEPFILES) | sed -e 's/\.o/.c/g' -e 's, , ../,g'`
	echo "Load .c corresponding to:" $(DEPFILES)


# This is useful when debugging GDB, because some Unix's don't let you run GDB
# on itself without copying the executable.  So "make gdb1" will make
# gdb and put a copy in gdb1, and you can run it with "gdb gdb1".
# Removing gdb1 before the copy is the right thing if gdb1 is open
# in another process.
gdb1: gdb
	rm -f gdb1
	cp gdb gdb1

# This is a remote stub which runs under unix and starts up an
# inferior process.  This is at least useful for debugging GDB's
# remote support.
rapp: $(RAPP_OBS)
	rm -f rapp_init.c
	${srcdir}/munch ${MUNCH_DEFINE} ${RAPP_OBS} > rapp_init.c
	${CC-LD} $(LDFLAGS) -o $@ rapp_init.c $(RAPP_OBS)
	
# Support for building Makefile out of configured pieces, automatically
# generated dependencies, etc.  alldeps.mak is a file that contains
# "make" variable definitions for all ALLDEPFILES, ALLDEPFILES_MAINDIR,
# ALLDEPFILES_SUBDIR, ALLPARAM, and ALLCONFIG, all cadged from the current
# contents of the config subdirectory.

alldeps.mak: ${srcdir}/config
	rm -f alldeps.mak alldeps.tmp allparam.tmp allconfig.tmp
	for i in `ls -d ${srcdir}/config/*.m[ht]` ; do \
	  echo $$i >>allconfig.tmp; \
	  awk <$$i ' \
	    $$1 == "TDEPFILES=" || $$1 == "XDEPFILES=" \
		|| $$1 == "NATDEPFILES=" { \
	      for (i = 2; i <= NF; i++) \
	        print $$i >> "alldeps.tmp" ; \
            } \
	    $$1 == "TM_FILE=" || $$1 == "XM_FILE=" || $$1 == "NAT_FILE" { \
	      print $$2 >> "allparam.tmp" }' ; \
	done
	sort <alldeps.tmp | uniq | \
	  sed -e 's/arm-convert.o/arm-convert.s/' \
	      -e 's!^Onindy.o!nindy-share/Onindy.c!' \
	      -e 's!^nindy.o!nindy-share/nindy.c!' \
	      -e 's!ttybreak.o!nindy-share/ttybreak.c!' \
	      -e 's!ttyflush.o!nindy-share/ttyflush.c!' \
	      -e 's!xdr_ld.o!vx-share/xdr_ld.c!' \
	      -e 's!xdr_ptrace.o!vx-share/xdr_ptrace.c!' \
	      -e 's!xdr_rdb.o!vx-share/xdr_rdb.c!' \
	      -e 's!xdr_regs.o!vx-share/xdr_regs.c!' \
	      -e 's/\.o/.c/' \
	    >alldeps2.tmp
	echo '# Start of "alldeps.mak" definitions' \
	    >>alldeps.mak;
	echo 'ALLDEPFILES = $$(ALLDEPFILES_MAINDIR) $$(ALLDEPFILES_SUBDIR)' \
	    >>alldeps.mak;
	grep -v / alldeps2.tmp | \
	  awk 'BEGIN {printf "ALLDEPFILES_MAINDIR="} \
	    NR == 0 {printf $$0;} \
	    NR != 0 {printf "\\\n" $$0} \
	    END {printf "\n\n"}' >>alldeps.mak;
	grep / alldeps2.tmp | \
	  awk 'BEGIN {printf "ALLDEPFILES_SUBDIR="} \
	    NR == 0 {printf $$0;} \
	    NR != 0 {printf "\\\n" $$0} \
	    END {printf "\n\n"}' >>alldeps.mak;
	sort <allparam.tmp | uniq | awk 'BEGIN {printf "ALLPARAM="} \
	    NR == 0 {printf $$0;} \
	    NR != 0 {printf "\\\n" $$0} \
	    END {printf "\n\n"}' >>alldeps.mak;
	sort <allconfig.tmp | uniq | awk 'BEGIN {printf "ALLCONFIG="} \
	    NR == 0 {printf $$0;} \
	    NR != 0 {printf "\\\n" $$0} \
	    END {printf "\n\n"}' >>alldeps.mak;
	echo '# End of "alldeps.mak" definitions' \
	    >>alldeps.mak;
	rm -f alldeps.tmp alldeps2.tmp allparam.tmp allconfig.tmp

# The sed script makes everything which depends on {x,t}m.h depend on
# config.status as well, in case someone reconfigures gdb out from
# under an already compiled gdb.
depend: $(SOURCES) Makefile.in
	@echo Ignore errors about non-existent system-supplied include files
	@echo for systems other than the one you are using.
	@echo Also ignore parse errors in valops.c, and any errors in
	@echo arm-convert.s.
	touch xm.h tm.h
	-$(GCC) -MM -nostdinc -I/usr/include $(CFLAGS) -I$(BFD_DIR) \
	  -I${READLINE_DIR} $(INCLUDE_CFLAGS) \
	  `ls $(SOURCES) | grep -v '\.[hy]$$' | \
	  sort -u` >depend.tmp
# If running in srcdir, translate "./foo.c" into "$srcdir/foo.c" except
# for xm.h and tm.h.  This allows the same "depend" file to be used
# by the various subdirectories.
	if [ "${srcdir}" = "." ] ; then \
	  <depend.tmp sed \
	    -e 's; ./xm.h; xm.h;g'  \
	    -e 's; ./tm.h; tm.h;g'  \
	    -e 's; \./; $${srcdir}/;g'  \
	    -e 's; vx-share/; $${srcdir}/vx-share/;g'  \
	    -e 's; nindy-share/; $${srcdir}/nindy-share/;g'  \
	    >depend.tm2; \
	  rm depend.tmp; \
	  mv depend.tm2 depend.tmp; \
	fi
	<depend.tmp sed \
	  -e 's; [xt]m.h;& config.status;g'  \
	  -e 's; $(INCLUDE_DIR)/; $(INCLUDE_DEP)/;g'  \
	  -e 's; $(READLINE_DIR)/; $(READLINE_DEP)/;g'  \
	  -e 's; [a-z0-9./]*bfd/; $(BFD_DEP)/;g'  \
	  -e 's; ./xm.h; xm.h config.status;g'  \
	  -e 's; ./tm.h; tm.h config.status;g'  \
	  >depend
	rm depend.tmp

config.status:
	@echo "You must configure gdb.  Look at the README file for details."
	@false

# This checks the configure.in file versus the config/ directory.
config-check: config-check-hosts config-check-targets
config-check-hosts:
	grep gdb_host= ${srcdir}/configure.in | \
		sed -e 's/.*gdb_host=//' -e 's/ ;;$$/.mh/' | sort -u >HOSTconf.o
	(cd ${srcdir}/config; ls *.mh) >HOSTdir.o
	diff -u HOSTconf.o HOSTdir.o

config-check-targets:
	grep gdb_target= ${srcdir}/configure.in | \
		sed -e 's/.*gdb_target=//' -e 's/ ;;$$/.mh/' | sort -u >TARGconf.o
	(cd ${srcdir}/config; ls *.mt) >TARGdir.o
	diff -u HOSTconf.o HOSTdir.o

# These are not generated by "make depend" because they only are there
# for some machines.
# But these rules don't do what we want; we want to hack the foo.o: tm.h
# dependency to do the right thing.
tm-isi.h tm-sun3.h tm-news.h tm-hp300bsd.h tm-altos.h: tm-68k.h
tm-hp300hpux.h tm-sun2.h tm-3b1.h: tm-68k.h
xm-news1000.h: xm-news.h
xm-i386-sv32.h: xm-i386.h
tm-i386gas.h: tm-i386.h
xm-sun4os4.h: xm-sparc.h
tm-sun4os4.h: tm-sparc.h
xm-vaxult.h: xm-vax.h
xm-vaxbsd.h: xm-vax.h

kdb: $(NTSSTART) $(OBS) $(NTSOBS) ${ADD_DEPS} ${CDEPS}
	rm -f init.c
	$(srcdir)/munch ${MUNCH_DEFINE} $(OBS) $(NTSOBS) > init.c
	$(CC) $(LDFLAGS) -c init.c $(CLIBS) 
	ld -o kdb $(NTSSTART) $(OBS) $(NTSOBS) init.o $(ADD_FILES) \
	  -lc $(CLIBS)

# Put the proper machine-specific files first.
# createtags will edit the .o in DEPFILES into .c
TAGS: ${TAGFILES}
	$(srcdir)/createtags $(TM_FILE) ${XM_FILE} ${NAT_FILE} \
			     $(DEPFILES) ${TAGFILES}
tags: TAGS

# Making distributions of GDB and friends.

# Make a directory `proto-gdb.dir' that contains an image of the GDB
# directory of the distribution, built up with symlinks.
make-proto-gdb.dir: force_update 
	$(MAKE) $(MFLAGS) -f Makefile.in setup-to-dist
	$(MAKE) $(MFLAGS) -f Makefile    make-proto-gdb-1

# Make a tar file containing the GDB directory of the distribution.
gdb.tar.Z: force_update
	$(MAKE) $(MFLAGS) -f Makefile.in setup-to-dist
	$(MAKE) $(MFLAGS) -f Makefile.in gdb-$(VERSION).tar.Z

# Update the "alldeps.mak" file in a source directory.
update-alldeps: force_update
	../configure none -norecursion
	rm -f alldeps.mak
	$(MAKE) $(MFLAGS) alldeps.mak

# Update the "depend" and "alldeps.mak" files in a source directory.
# We update alldeps.mak first, since it is used to generate the list
# of files to be checked for dependencies.
update-depend: update-alldeps force_update
	../configure none -norecursion
	rm -f depend
	$(MAKE) $(MFLAGS) depend

# Set up the GDB source directory for distribution, by building all files that
# are products of other files.
setup-to-dist: update-depend force_update
	../configure none
	(cd doc; $(MAKE) $(MFLAGS) gdbVN.m4)
	$(MAKE) $(MFLAGS) gdb.info
	$(MAKE) $(MFLAGS) refcard.ps

# Build a tar file from a proto-gdb.dir.
gdb-$(VERSION).tar.Z: force_update
	rm -f gdb.tar gdb-$(VERSION).tar.Z
	$(MAKE) $(MFLAGS) -f Makefile    make-proto-gdb-1
	ln -s proto-gdb.dir $(DIST)
	tar chf - $(DIST) | compress >gdb-$(VERSION).tar.Z
	rm -rf $(DIST) proto-gdb.dir

# Build a proto-gdb.dir after GDB has been set up for distribution.
# This stuff must be run in `Makefile', not `Makefile.in`; we use the makefile
# built in the setup-to-dist process, since it defines things like ALLCONFIG
# and ALLDEPFILES, that we need.
make-proto-gdb-1: ${TARFILES} ${TARDIRS} gdb.info
	rm -rf proto-gdb.dir
	mkdir proto-gdb.dir
	cd proto-gdb.dir ; for i in ${TARFILES} ; do ln -s ../$$i . ; done
	cd proto-gdb.dir ; ln -s ../${INFOFILES} .
	cd proto-gdb.dir ; for i in ${TARDIRS}; do \
	  mkdir $$i; cd $$i; \
	  ln -s ../../$$i/* .; \
	  rm -rf SCCS CVS.adm RCS config.status; \
	  if test -f Makefile.in; then rm Makefile; else true; fi; done
	mkdir proto-gdb.dir/config
	cd proto-gdb.dir/config ; \
	  for i in $(ALLCONFIG) ; do ln -s ../../$$i ../$$i ; done
	mkdir proto-gdb.dir/vx-share proto-gdb.dir/nindy-share
	cd proto-gdb.dir/config ; \
	  for i in $(SFILES_SUBDIR) $(NONSRC_SUBDIR) $(ALLDEPFILES_SUBDIR); \
	    do ln -s ../../$$i ../$$i ; done
	chmod og=u `find . -print`

clean:
	rm -f *.o ${ADD_FILES} *~
	rm -f init.c version.c
	rm -f gdb core gdb.tar gdb.tar.Z make.log
	rm -f gdb[0-9]
	rm -f xgdb.o xgdb
	@$(MAKE) subdir_do DO=clean "DODIRS=$(SUBDIRS)" $(FLAGS_TO_PASS)

distclean: clean c-exp.tab.c m2-exp.tab.c TAGS
	rm -f tm.h xm.h config.status
	rm -f y.output yacc.acts yacc.tmp
	rm -f ${TESTS} Makefile depend
	@$(MAKE) subdir_do DO=distclean "DODIRS=$(SUBDIRS)" $(FLAGS_TO_PASS)

realclean: clean
	rm -f c-exp.tab.c m2-exp.tab.c TAGS
	rm -f tm.h xm.h config.status
	rm -f Makefile depend
	@$(MAKE) subdir_do DO=realclean "DODIRS=$(SUBDIRS)" $(FLAGS_TO_PASS)

STAGESTUFF=${OBS} ${TSOBS} ${NTSOBS} ${ADD_FILES} init.c init.o version.c gdb

subdir_do: force
	@for i in $(DODIRS); do \
		if [ -d ./$$i ] ; then \
			if (cd ./$$i; \
				$(MAKE) $(FLAGS_TO_PASS) $(DO)) ; then true ; \
			else exit 1 ; fi ; \
		else true ; fi ; \
	done

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

force:

# Documentation!
# GDB QUICK REFERENCE (TeX dvi file, CM fonts)
refcard.dvi: $(srcdir)/doc/refcard.tex
	( cd ./doc; $(MAKE) refcard.dvi $(FLAGS_TO_PASS) )
	mv ./doc/refcard.dvi .

# GDB QUICK REFERENCE (PostScript output, common PS fonts)
refcard.ps: $(srcdir)/doc/refcard.tex
	( cd ./doc; $(MAKE) refcard.ps $(FLAGS_TO_PASS) )
	mv ./doc/refcard.ps .

# GDB MANUAL: TeX dvi file
gdb.dvi: ./doc/gdb-all.texi
	( cd ./doc; $(MAKE) M4=$(M4) gdb.dvi $(FLAGS_TO_PASS) )
	mv ./doc/gdb.dvi .

# GDB MANUAL: info file
gdb.info: ./doc/gdb-all.texi
	( cd ./doc; $(MAKE) M4=$(M4) gdb.info $(FLAGS_TO_PASS) )
	mv ./doc/gdb.info* .

./doc/gdb-all.texi:
	(cd ./doc; $(MAKE) M4=$(M4) gdb-all.texi $(FLAGS_TO_PASS) )

# Make copying.c from COPYING
copying.c: ${srcdir}/COPYING ${srcdir}/copying.awk
	awk -f ${srcdir}/copying.awk < ${srcdir}/COPYING > copying.c

version.c: Makefile
	echo 'char *version = "$(VERSION)";' >version.c

# c-exp.tab.c is generated in target dir from c-exp.y if it doesn't exist
# in srcdir, then compiled in target dir to c-exp.tab.o.
# Remove bogus decls for malloc/realloc/free which conflict with everything
# else.
c-exp.tab.o: c-exp.tab.c
c-exp.tab.c: $(srcdir)/c-exp.y
	@echo 'Expect 4 shift/reduce conflicts.'
	${YACC} $(srcdir)/c-exp.y
	-sed -e '/extern.*malloc/d' \
	     -e '/extern.*realloc/d' \
	     -e '/extern.*free/d' \
	  < y.tab.c > c-exp.tab.c
	-rm y.tab.c

# m2-exp.tab.c is generated in target dir from m2-exp.y if it doesn't exist
# in srcdir, then compiled in target dir to m2-exp.tab.o.
m2-exp.tab.o: m2-exp.tab.c
m2-exp.tab.c: $(srcdir)/m2-exp.y
	${YACC} $(srcdir)/m2-exp.y
	-sed -e '/extern.*malloc/d' \
	     -e '/extern.*realloc/d' \
	     -e '/extern.*free/d' \
	  < y.tab.c > m2-exp.tab.c
	-rm y.tab.c

xgdb.o: defs.h symtab.h frame.h

main.o:	${srcdir}/main.c
	${CC} -c ${INTERNAL_CFLAGS} -I${READLINE_DIR} $<

# The symbol-file readers have dependencies on BFD header files.
dbxread.o: ${srcdir}/dbxread.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/dbxread.c

coffread.o: ${srcdir}/coffread.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/coffread.c

mipsread.o: ${srcdir}/mipsread.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/mipsread.c

elfread.o: ${srcdir}/elfread.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/elfread.c

dwarfread.o: ${srcdir}/dwarfread.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/dwarfread.c

stabsread.o: ${srcdir}/stabsread.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/stabsread.c

xcoffread.o: ${srcdir}/xcoffread.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/xcoffread.c

xcoffexec.o: ${srcdir}/xcoffexec.c
	${CC} -c ${INTERNAL_CFLAGS} -I$(BFD_DIR) ${srcdir}/xcoffexec.c

# Drag in the files that are in another directory.

xdr_ld.o: ${srcdir}/vx-share/xdr_ld.c
	${CC} -c ${INTERNAL_CFLAGS} ${srcdir}/vx-share/xdr_ld.c

xdr_ptrace.o: ${srcdir}/vx-share/xdr_ptrace.c
	${CC} -c ${INTERNAL_CFLAGS} ${srcdir}/vx-share/xdr_ptrace.c

xdr_rdb.o: ${srcdir}/vx-share/xdr_rdb.c
	${CC} -c ${INTERNAL_CFLAGS} ${srcdir}/vx-share/xdr_rdb.c

xdr_regs.o: ${srcdir}/vx-share/xdr_regs.c
	${CC} -c ${INTERNAL_CFLAGS} ${srcdir}/vx-share/xdr_regs.c

nindy.o: ${srcdir}/nindy-share/nindy.c
	${CC} -c ${INTERNAL_CFLAGS} -DSTRIP='"$(tooldir)/bin/strip"' \
		${srcdir}/nindy-share/nindy.c

Onindy.o: ${srcdir}/nindy-share/Onindy.c
	${CC} -c ${INTERNAL_CFLAGS} ${srcdir}/nindy-share/Onindy.c

ttybreak.o: ${srcdir}/nindy-share/ttybreak.c
	${CC} -c ${INTERNAL_CFLAGS} ${srcdir}/nindy-share/ttybreak.c

ttyflush.o: ${srcdir}/nindy-share/ttyflush.c
	${CC} -c ${INTERNAL_CFLAGS} ${srcdir}/nindy-share/ttyflush.c

lint: $(LINTFILES)
	$(LINT) $(INCLUDE_CFLAGS) $(LINTFLAGS) $(LINTFILES) \
	   `echo ${DEPFILES} | sed 's/\.o /\.c /g'

gdb.cxref: $(SFILES)
	cxref -I. $(SFILES) >gdb.cxref

force_update:

# GNU Make has an annoying habit of putting *all* the Makefile variables
# into the environment, unless you include this target as a circumvention.
# Rumor is that this will be fixed (and this target can be removed)
# in GNU Make 4.0.
.NOEXPORT:

# This is the end of "Makefile.in".  When built into "Makefile"
# by the configure script, two things are added below this point:
# 	alldeps.mak -- defintions of all files that are used in
#			host- or target-dependent configurations
#	depend -- what .o files depend on what .c and .h files,
#			for all configurations.


# Start of "alldeps.mak" definitions
ALLDEPFILES = $(ALLDEPFILES_MAINDIR) $(ALLDEPFILES_SUBDIR)
ALLDEPFILES_MAINDIR=\
a68v-xdep.c\
altos-xdep.c\
am29k-pinsn.c\
am29k-tdep.c\
arm-convert.s\
arm-pinsn.c\
arm-tdep.c\
arm-xdep.c\
convex-pinsn.c\
convex-tdep.c\
convex-xdep.c\
coredep.c\
corelow.c\
exec.c\
fork-child.c\
go32-nat.c\
go32-xdep.c\
gould-pinsn.c\
gould-xdep.c\
h8300-tdep.c\
hp300ux-xdep.c\
hppa-pinsn.c\
hppab-core.c\
hppab-nat.c\
hppab-tdep.c\
hppah-tdep.c\
i386-pinsn.c\
i386-tdep.c\
i386b-nat.c\
i386mach-nat.c\
i386v-nat.c\
i386v4-nat.c\
i387-tdep.c\
i960-pinsn.c\
i960-tdep.c\
infptrace.c\
inftarg.c\
irix4-nat.c\
m68k-pinsn.c\
m68k-tdep.c\
m88k-nat.c\
m88k-pinsn.c\
m88k-tdep.c\
mips-nat.c\
mips-pinsn.c\
mips-tdep.c\
news-xdep.c\
nindy-tdep.c\
ns32k-pinsn.c\
procfs.c\
pyr-pinsn.c\
pyr-tdep.c\
pyr-xdep.c\
remote-adapt.c\
remote-eb.c\
remote-es1800.c\
remote-hms.c\
remote-mm.c\
remote-nindy.c\
remote-st2000.c\
remote-vx.c\
rs6000-nat.c\
rs6000-pinsn.c\
rs6000-tdep.c\
ser-bsd.c\
ser-go32.c\
solib.c\
sparc-nat.c\
sparc-pinsn.c\
sparc-tdep.c\
sun3-nat.c\
sun386-nat.c\
symm-tdep.c\
symm-xdep.c\
tahoe-pinsn.c\
ultra3-nat.c\
ultra3-xdep.c\
umax-xdep.c\
vax-pinsn.c\
xcoffexec.c\
xcoffsolib.c

ALLDEPFILES_SUBDIR=\
nindy-share/Onindy.c\
nindy-share/nindy.c\
nindy-share/ttybreak.c\
nindy-share/ttyflush.c\
vx-share/xdr_ld.c\
vx-share/xdr_ptrace.c\
vx-share/xdr_rdb.c\
vx-share/xdr_regs.c

ALLPARAM=\
tm-29k.h\
tm-3b1.h\
tm-68k-noun.h\
tm-68k-un.h\
tm-altos.h\
tm-amix.h\
tm-arm.h\
tm-bigmips.h\
tm-convex.h\
tm-delta88.h\
tm-es1800.h\
tm-h8300.h\
tm-hp300bsd.h\
tm-hp300hpux.h\
tm-hppab.h\
tm-hppah.h\
tm-i386bsd.h\
tm-i386v.h\
tm-i386v4.h\
tm-irix3.h\
tm-isi.h\
tm-linux.h\
tm-m88k.h\
tm-merlin.h\
tm-mips.h\
tm-news.h\
tm-nindy960.h\
tm-np1.h\
tm-os68k.h\
tm-pn.h\
tm-pyr.h\
tm-rs6000.h\
tm-spc-noun.h\
tm-spc-un.h\
tm-st2000.h\
tm-sun2.h\
tm-sun2os4.h\
tm-sun3.h\
tm-sun386.h\
tm-sun3os4.h\
tm-sun4os4.h\
tm-sun4sol2.h\
tm-symmetry.h\
tm-tahoe.h\
tm-ultra3.h\
tm-umax.h\
tm-vax.h\
tm-vx68.h\
tm-vx960.h\
xm-3b1.h\
xm-altos.h\
xm-amix.h\
xm-apollo68b.h\
xm-apollo68v.h\
xm-arm.h\
xm-bigmips.h\
xm-convex.h\
xm-delta88.h\
xm-go32.h\
xm-hp300bsd.h\
xm-hp300hpux.h\
xm-hppab.h\
xm-hppah.h\
xm-i386bsd.h\
xm-i386mach.h\
xm-i386sco.h\
xm-i386v.h\
xm-i386v32.h\
xm-i386v4.h\
xm-irix3.h\
xm-irix4.h\
xm-isi.h\
xm-linux.h\
xm-m88k.h\
xm-merlin.h\
xm-mips.h\
xm-news.h\
xm-news1000.h\
xm-np1.h\
xm-pn.h\
xm-pyr.h\
xm-rs6000.h\
xm-rtbsd.h\
xm-sun2.h\
xm-sun3.h\
xm-sun386.h\
xm-sun3os4.h\
xm-sun4os4.h\
xm-sun4sol2.h\
xm-symmetry.h\
xm-tahoe.h\
xm-ultra3.h\
xm-umax.h\
xm-vaxbsd.h\
xm-vaxult.h

ALLCONFIG=\
./config/3b1.mh\
./config/3b1.mt\
./config/a29k-kern.mt\
./config/a29k.mt\
./config/altos.mh\
./config/altos.mt\
./config/amix.mh\
./config/amix.mt\
./config/apollo68b.mh\
./config/apollo68v.mh\
./config/arm.mh\
./config/arm.mt\
./config/bigmips.mh\
./config/bigmips.mt\
./config/convex.mh\
./config/convex.mt\
./config/decstation.mh\
./config/decstation.mt\
./config/delta88.mh\
./config/delta88.mt\
./config/es1800.mt\
./config/go32.mh\
./config/h8300hms.mt\
./config/hp300bsd.mh\
./config/hp300bsd.mt\
./config/hp300hpux.mh\
./config/hp300hpux.mt\
./config/hppabsd.mh\
./config/hppabsd.mt\
./config/hppahpux.mh\
./config/hppahpux.mt\
./config/i386aout.mt\
./config/i386bsd.mh\
./config/i386bsd.mt\
./config/i386mach.mh\
./config/i386sco.mh\
./config/i386sol2.mh\
./config/i386sol2.mt\
./config/i386v.mh\
./config/i386v.mt\
./config/i386v32.mh\
./config/i386v4.mh\
./config/i386v4.mt\
./config/irix3.mh\
./config/irix3.mt\
./config/irix4.mh\
./config/isi.mh\
./config/isi.mt\
./config/linux.mh\
./config/linux.mt\
./config/littlemips.mh\
./config/littlemips.mt\
./config/m68k-noun.mt\
./config/m68k-un.mt\
./config/m88k.mh\
./config/m88k.mt\
./config/merlin.mh\
./config/merlin.mt\
./config/ncr3000.mh\
./config/ncr3000.mt\
./config/news.mh\
./config/news.mt\
./config/news1000.mh\
./config/nindy960.mt\
./config/none.mh\
./config/none.mt\
./config/np1.mh\
./config/np1.mt\
./config/os68k.mt\
./config/pn.mh\
./config/pn.mt\
./config/pyramid.mh\
./config/pyramid.mt\
./config/rs6000.mh\
./config/rs6000.mt\
./config/rtbsd.mh\
./config/sparc-noun.mt\
./config/sparc-un.mt\
./config/st2000.mt\
./config/sun2os3.mh\
./config/sun2os3.mt\
./config/sun2os4.mh\
./config/sun2os4.mt\
./config/sun386.mh\
./config/sun386.mt\
./config/sun3os3.mh\
./config/sun3os3.mt\
./config/sun3os4.mh\
./config/sun3os4.mt\
./config/sun4os4.mh\
./config/sun4os4.mt\
./config/sun4sol2.mh\
./config/sun4sol2.mt\
./config/symmetry.mh\
./config/symmetry.mt\
./config/tahoe.mh\
./config/tahoe.mt\
./config/ultra3.mh\
./config/ultra3.mt\
./config/umax.mh\
./config/umax.mt\
./config/vax.mt\
./config/vaxbsd.mh\
./config/vaxult.mh\
./config/vxworks68.mt\
./config/vxworks960.mt

# End of "alldeps.mak" definitions
a68v-xdep.o : a68v-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
altos-xdep.o : altos-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
am29k-pinsn.o : am29k-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status target.h ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/a29k.h 
am29k-tdep.o : am29k-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status gdbcore.h ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h frame.h value.h symtab.h \
  gdbtypes.h expression.h inferior.h breakpoint.h gdbcmd.h command.h 
arm-pinsn.o : arm-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/arm.h 
arm-tdep.o : arm-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h arm-opcode.h gdbcore.h ${srcdir}/../include/bfd.h 
arm-xdep.o : arm-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h arm-opcode.h gdbcore.h ${srcdir}/../include/bfd.h 
blockframe.o : blockframe.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/bfd.h symfile.h objfiles.h \
  frame.h gdbcore.h value.h gdbtypes.h expression.h target.h inferior.h breakpoint.h 
breakpoint.o : breakpoint.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h frame.h breakpoint.h value.h gdbtypes.h \
  expression.h gdbcore.h ${srcdir}/../include/bfd.h gdbcmd.h command.h inferior.h target.h \
  language.h ${srcdir}/../include/demangle.h 
buildsym.o : buildsym.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h symtab.h symfile.h objfiles.h \
  buildsym.h 
c-exp.tab.o : c-exp.tab.c ${srcdir}/defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/symtab.h ${srcdir}/../include/obstack.h ${srcdir}/gdbtypes.h ${srcdir}/frame.h ${srcdir}/expression.h \
  ${srcdir}/parser-defs.h ${srcdir}/value.h ${srcdir}/language.h ${srcdir}/../include/bfd.h ${srcdir}/symfile.h ${srcdir}/objfiles.h 
coffread.o : coffread.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h breakpoint.h frame.h value.h \
  expression.h ${srcdir}/../include/bfd.h symfile.h objfiles.h buildsym.h ${srcdir}/../include/coff/internal.h \
  ${srcdir}/../bfd/libcoff.h 
command.o : command.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status gdbcmd.h command.h symtab.h ${srcdir}/../include/obstack.h value.h gdbtypes.h expression.h 
convex-pinsn.o : convex-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h convx-opcode.h 
convex-tdep.o : convex-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status command.h symtab.h ${srcdir}/../include/obstack.h value.h gdbtypes.h expression.h \
  frame.h inferior.h breakpoint.h ${srcdir}/../include/wait.h gdbcore.h ${srcdir}/../include/bfd.h \
  gdbcmd.h 
convex-xdep.o : convex-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status command.h symtab.h ${srcdir}/../include/obstack.h value.h gdbtypes.h expression.h \
  frame.h inferior.h breakpoint.h ${srcdir}/../include/wait.h gdbcore.h ${srcdir}/../include/bfd.h \
  gdbcmd.h 
core.o : core.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h command.h ${srcdir}/../include/bfd.h target.h gdbcore.h 
coredep.o : coredep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status gdbcore.h ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h nm.h 
corelow.o : corelow.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h command.h ${srcdir}/../include/bfd.h target.h gdbcore.h 
dbxread.o : dbxread.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h breakpoint.h frame.h value.h gdbtypes.h expression.h command.h \
  target.h ${srcdir}/../include/bfd.h gdbcore.h ${srcdir}/../bfd/libbfd.h ${srcdir}/../bfd/libaout.h symfile.h \
  objfiles.h buildsym.h stabsread.h gdb-stabs.h ${srcdir}/../include/demangle.h ${srcdir}/../include/aout/aout64.h \
  ${srcdir}/../include/aout/stab_gnu.h ${srcdir}/../include/aout/stab.def partial-stab.h 
demangle.o : demangle.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status command.h gdbcmd.h ${srcdir}/../include/demangle.h 
dwarfread.o : dwarfread.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h symtab.h gdbtypes.h symfile.h \
  objfiles.h ${srcdir}/../bfd/libbfd.h ${srcdir}/../include/elf/dwarf.h buildsym.h ${srcdir}/../include/demangle.h 
elfread.o : elfread.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/elf/common.h ${srcdir}/../include/elf/external.h ${srcdir}/../include/elf/internal.h \
  ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h ${srcdir}/../bfd/libbfd.h symtab.h symfile.h \
  objfiles.h buildsym.h gdb-stabs.h 
environ.o : environ.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status environ.h 
eval.o : eval.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h value.h expression.h target.h \
  ${srcdir}/../include/bfd.h frame.h 
exec.o : exec.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h gdbcmd.h command.h gdbcore.h 
expprint.o : expprint.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h expression.h value.h language.h \
  parser-defs.h 
findvar.o : findvar.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h frame.h value.h expression.h \
  gdbcore.h ${srcdir}/../include/bfd.h inferior.h breakpoint.h target.h 
fork-child.o : fork-child.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h ${srcdir}/../include/wait.h gdbcore.h \
  terminal.h 
gdbtypes.o : gdbtypes.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h symtab.h symfile.h objfiles.h \
  gdbtypes.h expression.h language.h target.h value.h ${srcdir}/../include/demangle.h 
go32-nat.o : go32-nat.c 
go32-xdep.o : go32-xdep.c 
gould-pinsn.o : gould-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h frame.h gdbcore.h ${srcdir}/../include/bfd.h ${srcdir}/../include/opcode/np1.h 
gould-xdep.o : gould-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
h8300-tdep.o : h8300-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h ${srcdir}/../include/obstack.h symtab.h 
hp300ux-xdep.o : hp300ux-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
hppa-pinsn.o : hppa-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/hppa.h 
hppab-core.o : hppab-core.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h /usr/src/sys/hpux/hpux.h 
hppab-nat.o : hppab-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h 
hppab-tdep.o : hppab-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h gdbcmd.h command.h 
hppah-tdep.o : hppah-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h gdbcmd.h command.h 
i386-pinsn.o : i386-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status gdbcore.h ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h 
i386-tdep.o : i386-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h target.h 
i386b-nat.o : i386b-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status 
i386mach-nat.o : i386mach-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
i386v-nat.o : i386v-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h language.h gdbcore.h ${srcdir}/../include/bfd.h ieee-float.h \
  target.h 
i386v4-nat.o : i386v4-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status 
i387-tdep.o : i387-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h language.h gdbcore.h ${srcdir}/../include/bfd.h ieee-float.h 
i960-pinsn.o : i960-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h 
i960-tdep.o : i960-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h value.h gdbtypes.h expression.h frame.h \
  ieee-float.h 
ieee-float.o : ieee-float.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ieee-float.h 
infcmd.o : infcmd.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h frame.h inferior.h breakpoint.h \
  value.h expression.h environ.h gdbcmd.h command.h gdbcore.h ${srcdir}/../include/bfd.h \
  target.h 
inflow.o : inflow.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h command.h signals.h terminal.h target.h ${srcdir}/../include/bfd.h 
infptrace.o : infptrace.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h nm.h gdbcore.h 
infrun.o : infrun.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h frame.h inferior.h breakpoint.h value.h \
  gdbtypes.h expression.h ${srcdir}/../include/wait.h gdbcore.h ${srcdir}/../include/bfd.h gdbcmd.h \
  command.h target.h 
inftarg.o : inftarg.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h ${srcdir}/../include/wait.h gdbcore.h \
  terminal.h 
irix4-nat.o : irix4-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h 
language.o : language.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h value.h expression.h gdbcmd.h \
  command.h frame.h language.h target.h ${srcdir}/../include/bfd.h parser-defs.h 
m2-exp.tab.o : m2-exp.tab.c ${srcdir}/defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/symtab.h ${srcdir}/../include/obstack.h ${srcdir}/gdbtypes.h ${srcdir}/frame.h ${srcdir}/expression.h \
  ${srcdir}/language.h ${srcdir}/value.h ${srcdir}/parser-defs.h ${srcdir}/../include/bfd.h ${srcdir}/symfile.h ${srcdir}/objfiles.h 
m68k-pinsn.o : m68k-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/m68k.h gdbcore.h ${srcdir}/../include/bfd.h \
  ieee-float.h 
m68k-tdep.o : m68k-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ieee-float.h frame.h symtab.h ${srcdir}/../include/obstack.h 
m88k-nat.o : m88k-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h /usr/include/setjmp.h 
m88k-pinsn.o : m88k-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/opcode/m88k.h symtab.h ${srcdir}/../include/obstack.h 
m88k-tdep.o : m88k-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h /usr/include/setjmp.h 
main.o : main.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status gdbcmd.h command.h call-cmds.h symtab.h ${srcdir}/../include/obstack.h inferior.h \
  breakpoint.h frame.h value.h gdbtypes.h expression.h signals.h target.h ${srcdir}/../include/bfd.h \
  language.h ${srcdir}/../include/getopt.h ${srcdir}/../readline/readline.h ${srcdir}/../readline/keymaps.h \
  ${srcdir}/../readline/chardefs.h ${srcdir}/../readline/history.h 
maint.o : maint.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status command.h gdbcmd.h symtab.h ${srcdir}/../include/obstack.h gdbtypes.h ${srcdir}/../include/demangle.h 
mem-break.o : mem-break.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status 
minsyms.o : minsyms.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/bfd.h symfile.h objfiles.h 
mips-nat.o : mips-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
mips-pinsn.o : mips-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/mips.h 
mips-tdep.o : mips-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcmd.h command.h language.h gdbcore.h ${srcdir}/../include/bfd.h \
  symfile.h objfiles.h 
mipsread.o : mipsread.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm-mips.h ${srcdir}/../include/coff/sym.h ${srcdir}/../include/coff/symconst.h symtab.h ${srcdir}/../include/obstack.h \
  gdbtypes.h gdbcore.h ${srcdir}/../include/bfd.h symfile.h objfiles.h buildsym.h stabsread.h \
  gdb-stabs.h ${srcdir}/../include/coff/mips.h ${srcdir}/../include/coff/ecoff-ext.h ${srcdir}/../bfd/libbfd.h \
  ${srcdir}/../bfd/libaout.h ${srcdir}/../include/aout/aout64.h ${srcdir}/../include/aout/stab_gnu.h ${srcdir}/../include/aout/stab.def \
  partial-stab.h 
news-xdep.o : news-xdep.c 
Onindy.o : ${srcdir}/nindy-share/Onindy.c ${srcdir}/nindy-share/ttycntl.h ${srcdir}/nindy-share/block_io.h \
  ${srcdir}/../include/wait.h ${srcdir}/nindy-share/env.h /usr/include/string.h 
nindy.o : ${srcdir}/nindy-share/nindy.c ${srcdir}/nindy-share/ttycntl.h ${srcdir}/nindy-share/block_io.h ${srcdir}/../include/wait.h \
  ${srcdir}/nindy-share/env.h /usr/include/string.h 
ttybreak.o : ${srcdir}/nindy-share/ttybreak.c ${srcdir}/nindy-share/ttycntl.h 
ttyflush.o : ${srcdir}/nindy-share/ttyflush.c ${srcdir}/nindy-share/ttycntl.h 
nindy-tdep.o : nindy-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h frame.h 
ns32k-pinsn.o : ns32k-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ns32k-opcode.h gdbcore.h ${srcdir}/../include/bfd.h 
objfiles.o : objfiles.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h symtab.h symfile.h objfiles.h 
parse.o : parse.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h frame.h expression.h value.h \
  command.h language.h parser-defs.h 
printcmd.o : printcmd.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h symtab.h ${srcdir}/../include/obstack.h gdbtypes.h value.h expression.h \
  language.h gdbcore.h ${srcdir}/../include/bfd.h gdbcmd.h command.h target.h breakpoint.h \
  ${srcdir}/../include/demangle.h 
procfs.o : procfs.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h command.h gdbcore.h nm.h 
pyr-pinsn.o : pyr-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/pyr.h gdbcore.h ${srcdir}/../include/bfd.h 
pyr-tdep.o : pyr-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status 
pyr-xdep.o : pyr-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
remote-adapt.o : remote-adapt.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h ${srcdir}/../include/wait.h terminal.h target.h ${srcdir}/../include/bfd.h \
  gdbcore.h 
remote-eb.o : remote-eb.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm-29k.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h ${srcdir}/../include/wait.h terminal.h target.h ${srcdir}/../include/bfd.h \
  gdbcore.h 
remote-es1800.o : remote-es1800.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h ${srcdir}/../include/wait.h terminal.h \
  command.h 
remote-hms.o : remote-hms.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h ${srcdir}/../include/wait.h terminal.h target.h ${srcdir}/../include/bfd.h \
  gdbcore.h 
remote-mm.o : remote-mm.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h ${srcdir}/../include/wait.h terminal.h minimon.h target.h ${srcdir}/../include/bfd.h 
remote-nindy.o : remote-nindy.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h gdbcore.h command.h ieee-float.h \
  ${srcdir}/../include/wait.h ${srcdir}/nindy-share/ttycntl.h ${srcdir}/nindy-share/demux.h ${srcdir}/nindy-share/env.h \
  ${srcdir}/nindy-share/stop.h 
remote-st2000.o : remote-st2000.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status gdbcore.h ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h target.h ${srcdir}/../include/wait.h \
  serial.h 
remote-vx.o : remote-vx.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h ${srcdir}/../include/wait.h target.h ${srcdir}/../include/bfd.h gdbcore.h \
  command.h symfile.h ${srcdir}/vx-share/ptrace.h ${srcdir}/vx-share/xdr_ptrace.h ${srcdir}/vx-share/xdr_ld.h \
  ${srcdir}/vx-share/xdr_rdb.h ${srcdir}/vx-share/dbgRpcLib.h ${srcdir}/vx-share/reg.h 
remote.o : remote.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h ${srcdir}/../include/wait.h terminal.h \
  gdbcmd.h command.h 
rs6000-nat.o : rs6000-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h nm.h 
rs6000-pinsn.o : rs6000-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/opcode/rs6k.h 
rs6000-tdep.o : rs6000-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h 
ser-bsd.o : ser-bsd.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status serial.h 
ser-go32.o : ser-go32.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status serial.h 
solib.o : solib.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/bfd.h symfile.h objfiles.h \
  gdbcore.h command.h target.h frame.h regex.h inferior.h breakpoint.h value.h \
  gdbtypes.h expression.h 
source.o : source.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h expression.h language.h command.h gdbcmd.h \
  frame.h gdbcore.h ${srcdir}/../include/bfd.h regex.h symfile.h objfiles.h 
sparc-nat.o : sparc-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h nm.h 
sparc-pinsn.o : sparc-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/sparc.h gdbcore.h ${srcdir}/../include/bfd.h \
  target.h 
sparc-tdep.o : sparc-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h ieee-float.h nm.h gdbcore.h 
stabsread.o : stabsread.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h symtab.h gdbtypes.h symfile.h \
  objfiles.h ${srcdir}/../include/aout/stab_gnu.h ${srcdir}/../include/aout/stab.def buildsym.h \
  stabsread.h 
stack.o : stack.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status value.h symtab.h ${srcdir}/../include/obstack.h gdbtypes.h expression.h language.h \
  frame.h gdbcmd.h command.h gdbcore.h ${srcdir}/../include/bfd.h target.h breakpoint.h \
  ${srcdir}/../include/demangle.h 
sun3-nat.o : sun3-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h frame.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
sun386-nat.o : sun386-nat.c 
symfile.o : symfile.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h gdbcore.h ${srcdir}/../include/bfd.h \
  frame.h target.h value.h expression.h symfile.h objfiles.h gdbcmd.h command.h \
  breakpoint.h language.h 
symm-tdep.o : symm-tdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
symm-xdep.o : symm-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
symmisc.o : symmisc.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h ${srcdir}/../include/bfd.h symfile.h \
  objfiles.h breakpoint.h frame.h value.h expression.h command.h language.h 
symtab.o : symtab.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h gdbcore.h ${srcdir}/../include/bfd.h \
  frame.h target.h value.h expression.h symfile.h objfiles.h gdbcmd.h command.h \
  call-cmds.h regex.h language.h ${srcdir}/../include/demangle.h 
tahoe-pinsn.o : tahoe-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/tahoe.h 
target.o : target.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status target.h ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h gdbcmd.h command.h symtab.h \
  inferior.h breakpoint.h frame.h value.h gdbtypes.h expression.h symfile.h objfiles.h 
ultra3-nat.o : ultra3-nat.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
ultra3-xdep.o : ultra3-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
umax-xdep.o : umax-xdep.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h gdbcore.h ${srcdir}/../include/bfd.h 
utils.o : utils.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status signals.h gdbcmd.h command.h terminal.h ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h \
  target.h ${srcdir}/../include/demangle.h 
valarith.o : valarith.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status value.h symtab.h ${srcdir}/../include/obstack.h gdbtypes.h expression.h target.h \
  ${srcdir}/../include/bfd.h 
valops.o : valops.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h value.h expression.h frame.h \
  inferior.h breakpoint.h gdbcore.h ${srcdir}/../include/bfd.h target.h 
valprint.o : valprint.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h value.h expression.h gdbcore.h \
  ${srcdir}/../include/bfd.h gdbcmd.h command.h target.h language.h ${srcdir}/../include/demangle.h 
values.o : values.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h gdbtypes.h value.h expression.h gdbcore.h \
  ${srcdir}/../include/bfd.h frame.h command.h gdbcmd.h target.h ${srcdir}/../include/demangle.h 
vax-pinsn.o : vax-pinsn.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status symtab.h ${srcdir}/../include/obstack.h ${srcdir}/../include/opcode/vax.h 
xdr_ld.o : ${srcdir}/vx-share/xdr_ld.c ${srcdir}/vx-share/vxWorks.h ${srcdir}/vx-share/vxTypes.h /usr/include/rpc/rpc.h \
  ${srcdir}/vx-share/xdr_ld.h 
xdr_ptrace.o : ${srcdir}/vx-share/xdr_ptrace.c ${srcdir}/vx-share/vxWorks.h ${srcdir}/vx-share/vxTypes.h ${srcdir}/vx-share/xdr_ptrace.h 
xdr_rdb.o : ${srcdir}/vx-share/xdr_rdb.c ${srcdir}/vx-share/vxWorks.h ${srcdir}/vx-share/vxTypes.h ${srcdir}/vx-share/xdr_rdb.h 
xdr_regs.o : ${srcdir}/vx-share/xdr_regs.c ${srcdir}/vx-share/vxWorks.h ${srcdir}/vx-share/vxTypes.h ${srcdir}/vx-share/reg.h \
  ${srcdir}/vx-share/xdr_regs.h 
xcoffexec.o : xcoffexec.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status frame.h inferior.h symtab.h ${srcdir}/../include/obstack.h breakpoint.h value.h \
  gdbtypes.h expression.h target.h ${srcdir}/../include/bfd.h gdbcmd.h command.h gdbcore.h \
  symfile.h objfiles.h ${srcdir}/../bfd/libbfd.h xcoffsolib.h 
xcoffread.o : xcoffread.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h 
xcoffsolib.o : xcoffsolib.c defs.h ${srcdir}/../include/ansidecl.h xm.h config.status ${srcdir}/../include/fopen-same.h \
  tm.h config.status ${srcdir}/../include/bfd.h ${srcdir}/../include/obstack.h xcoffsolib.h 
