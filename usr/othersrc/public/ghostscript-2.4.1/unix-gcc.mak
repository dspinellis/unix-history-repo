#    Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# makefile for Ghostscript, Unix/gcc/X11 configuration.
# Note: this makefile assumes you are using gcc in ANSI mode.

# ------------------------------- Options ------------------------------- #

####### The following are the only parts of the file you should need to edit.

# ------ Generic options ------ #

# Define the default directory/ies for the runtime
# initialization and font files.  Separate multiple directories with a :.
# `pwd` means use the directory in which the 'make' is being done.

GS_LIB_DEFAULT=`pwd`:`pwd`/fonts

# Define the name of the Ghostscript initialization file.
# (There is no reason to change this.)

GS_INIT=gs_init.ps

# Choose generic configuration options.

# -DDEBUG
#	includes debugging features (-Z switch) in the code.
#	  Code runs substantially slower even if no debugging switches
#	  are set.
# -DNOPRIVATE
#	makes private (static) procedures and variables public,
#	  so they are visible to the debugger and profiler.
#	  No execution time or space penalty.

GENOPT=

# ------ Platform-specific options ------ #

# Define the name of the C compiler.

CC=gcc

# Define the other compilation flags.
# Add -DBSD4_2 for 4.2bsd systems.
# Add -DUSG (GNU convention) or -DSYSV for System V or DG/UX.
# Add -DSYSV -D__SVR3 for SCO ODT, ISC Unix 2.2 or before,
#   or any System III Unix, or System V release 3-or-older Unix.
# Add -DSVR4 (not -DSYSV) for System V release 4.
# XCFLAGS can be set from the command line.
# We don't include -ansi, because this gets in the way of the platform-
#   specific stuff that <math.h> typically needs; nevertheless, we expect
#   gcc to accept ANSI-style function prototypes and function definitions.

CFLAGS=-g -O $(XCFLAGS)

# Define platform flags for ld.
# SunOS and some others want -X; Ultrix wants -x.
# SunOS 4.n may need -Bstatic.
# XLDFLAGS can be set from the command line.

LDFLAGS=$(XLDFLAGS)

# Define any extra libraries to link into the executable.
# SCO ODT apparently needs -lsocket.
# (Libraries required by individual drivers are handled automatically.)

EXTRALIBS=

# Define the include switch(es) for the X11 header files.
# This can be null if handled in some other way (e.g., the files are
# in /usr/include, or the directory is supplied by an environment variable).

XINCLUDE=-I/usr/local/X/include

# Define the directory/ies for the X11 library files.
# This can be null if these files are in the default linker search path.

XLIBDIRS=-L/usr/local/X/lib

# Define the installation commands and target directories for
# executables and files.  Only relevant to `make install'.

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL) -m 775
INSTALL_DATA = $(INSTALL) -m 664

prefix = /usr/local
bindir = $(prefix)/bin
libdir = $(prefix)/lib/ghostscript

# ------ Devices and features ------ #

# Choose the language feature(s) to include.  See gs.mak for details.

FEATURE_DEVS=filter.dev dps.dev level2.dev

# Choose the device(s) to include.  See devs.mak for details.

DEVICE_DEVS=x11.dev

# ---------------------------- End of options --------------------------- #

# Define the name of the makefile -- used in dependencies.

MAKEFILE=unix-gcc.mak

# Define the ANSI-to-K&R dependency.  (gcc accepts ANSI syntax.)

AK=

# Define the compilation rules and flags.

CCC=$(CC) $(CCFLAGS) -c

# --------------------------- Generic makefile ---------------------------- #

# The remainder of the makefile (unixhead.mak, gs.mak, devs.mak, unixtail.mak)
# is generic.  tar_cat concatenates all these together.
#    Copyright (C) 1990, 1991 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Partial makefile for Ghostscript, common to all Unix configurations.

# This part of the makefile gets inserted after the compiler-specific part
# (xxx-head.mak) and before gs.mak and devs.mak.

# ----------------------------- Generic stuff ----------------------------- #

# Define the platform name.

PLATFORM=unix_

# Define the extensions for the object and executable files.

OBJ=o
XE=

# Define the need for uniq.

UNIQ=

# Define the current directory prefix, shell quote string, and shell names.

EXP=./
QQ=\"
SHELL=/bin/sh
SH=$(SHELL)
SHP=$(SH) $(EXP)

# Define the compilation rules and flags.

CCFLAGS=$(GENOPT) $(CFLAGS)

.c.o: $(AK)
	$(CCC) $*.c

CC0=$(CCC)
CCINT=$(CCC)
#    Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Generic makefile for Ghostscript.
# The platform-specific makefiles `include' this file.
# They define the following symbols:
#	GS_INIT - the name of the initialization file for Ghostscript,
#		normally gs_init.ps.
#	GS_LIB_DEFAULT - the default directory/ies for searching for the
#		initialization and font files at run time.
#	DEVICE_DEVS - the devices to include in the executable.
#		See devs.mak for details.
#	DEVICE_DEVS2...DEVICE_DEVS5 - additional devices, if the definition of
#		DEVICE_DEVS doesn't fit on one line.
#		See devs.mak for details.
#	FEATURE_DEVS - the optional features to include in the
#		executable.  Current features are:
#		    dps - support for Display PostScript extensions.
#			*** PARTIALLY IMPLEMENTED, SEE language.doc. ***
#		    level2 - support for PostScript Level 2 extensions.
#			*** PARTIALLY IMPLEMENTED, SEE language.doc. ***
#		    compfont - support for composite (type 0) fonts.
#			*** NOT IMPLEMENTED YET. ***
#		    filter - support for Level 2 filters (other than eexec,
#			ASCIIHexEncode/Decode, and PFBDecode, which are
#			always included).
#		    ccfonts - precompile fonts into C, and link them
#			with the executable.  In the standard makefiles,
#			this is only implemented for a very few fonts:
#			see fonts.doc for details.
# It is very unlikely that anyone would want to edit the remaining
#   symbols, but we describe them here for completeness:
#	PLATFORM - a "device" name for the platform, so that platforms can
#		add various kinds of resources like devices and features.
#	QQ - a " preceded by whatever escape characters are needed to
#		persuade the shell to pass a " to a program (" on MS-DOS,
#		\" on Unix).
#	XE - the extension for executable files (e.g., null or .exe).
#	OBJ - the extension for relocatable object files (e.g., o or obj).
#	CCC - the C invocation for normal compilation.
#	CC0 - a C invocation with the fewest possible flags.  Needed because
#		MS-DOS limits the length of command lines to 128 characters.
#	CCINT - the C invocation for compiling the main interpreter module,
#		normally the same as CCC: this is needed because the
#		Borland compiler generates *worse* code for this module
#		(but only this module) when optimization (-O) is turned on.
#	AK - if source files must be converted from ANSI to K&R syntax,
#		this is ansi2knr$(XE); if not, it is null.
#		If a particular platform requires other utility programs
#		to be built, AK must include them too.
#	UNIQ - null on systems that provide the uniq utility,
#		uniq$(XE) on systems where we have to provide our own.
#	SHP - the prefix for invoking a shell script in the current directory
#		(null for MS-DOS, $(SH) ./ for Unix).
#	EXPP, EXP - the prefix for invoking an executable program in the
#		current directory (null for MS-DOS, ./ for Unix).
#	SH - the shell for scripts (null on MS-DOS, sh on Unix).
# The platform-specific makefiles must also include rules for creating
#   ansi2knr$(XE) and genarch$(XE) from the corresponding .c files,
#   and for making arch.h by executing genarch$(XE).  (This
#   shouldn't really be necessary, but Turbo C and Unix C treat the -o
#   switch slightly differently (Turbo C requires no following space,
#   Unix C requires a following space), and I haven't found a way to capture
#   the difference in a macro; also, Unix requires ./ because . may not be
#   in the search path, whereas MS-DOS always looks in the current
#   directory first.)

all default: gs$(XE)

test: gt$(XE)

mostlyclean realclean distclean clean:
	rm -f *.$(OBJ) *.a core gmon.out
	rm -f *.dev gconfig.h obj*.tr lib*.tr
	rm -f t _temp_* _temp_*.* *.map *.sym
	rm -f ansi2knr$(XE) genarch$(XE) uniq$(XE) arch.h gs$(XE)

# Auxiliary programs

# genarch may cause a (deliberate) addressing fault,
# so we invoke it with a preceding -.

arch.h: genarch$(XE)
	- $(EXPP) $(EXP)genarch arch.h

# -------------------------------- Library -------------------------------- #

# Define the inter-dependencies of the .h files.
# Since not all versions of `make' defer expansion of macros,
# we must list these in bottom-to-top order.

# Generic files

arch_h=arch.h
std_h=std.h $(arch_h)
gs_h=gs.h $(std_h)
gx_h=gx.h $(gs_h)

# C library interfaces

# Because of variations in the "standard" header files between systems,
# we define local include files named *_.h to substitute for <*.h>.

vmsmath_h=vmsmath.h

dos__h=dos_.h
malloc__h=malloc_.h
math__h=math_.h $(vmsmath_h)
memory__h=memory_.h
stat__h=stat_.h
string__h=string_.h
time__h=time_.h

# Miscellaneous

gserrors_h=gserrors.h

GX=$(AK) $(gx_h)
GXERR=$(GX) $(gserrors_h)

###### Low-level facilities and utilities

### Include files

gschar_h=gschar.h
gscolor_h=gscolor.h
gscoord_h=gscoord.h
gsfont_h=gsfont.h
gsmatrix_h=gsmatrix.h
gspaint_h=gspaint.h
gspath_h=gspath.h
gsprops_h=gsprops.h
gsstate_h=gsstate.h $(gscolor_h)
gstype1_h=gstype1.h
gsutil_h=gsutil.h

gxarith_h=gxarith.h
gxbitmap_h=gxbitmap.h
gxcache_h=gxcache.h
gxchar_h=gxchar.h $(gschar_h)
gxclist_h=gxclist.h
gxcpath_h=gxcpath.h
gxdevice_h=gxdevice.h $(gsmatrix_h) $(gxbitmap_h)
gxdevmem_h=gxdevmem.h
gxfdir_h=gxfdir.h
gxfixed_h=gxfixed.h
gxfont_h=gxfont.h $(gsfont_h)
gximage_h=gximage.h
gxlum_h=gxlum.h
gxmatrix_h=gxmatrix.h $(gsmatrix_h)
gxop1_h=gxop1.h
gxpath_h=gxpath.h
gxtype1_h=gxtype1.h $(gstype1_h)

gzcolor_h=gzcolor.h $(gscolor_h)
gzdevice_h=gzdevice.h $(gxdevice_h)
gzht_h=gzht.h
gzline_h=gzline.h
gzpath_h=gzpath.h $(gxpath_h)
gzstate_h=gzstate.h $(gsstate_h)

### Executable code

gsutil.$(OBJ): gsutil.c \
  $(std_h) $(gsprops_h) $(gsutil_h)

gxcache.$(OBJ): gxcache.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gspaint_h) $(gzdevice_h) $(gzcolor_h) \
  $(gxcpath_h) $(gxdevmem_h) $(gxfont_h) $(gxfdir_h) $(gxchar_h) \
  $(gxcache_h) $(gzstate_h) $(gzpath_h)

gxclist.$(OBJ): gxclist.c $(GXERR) \
  $(gsmatrix_h) $(gxbitmap_h) $(gxclist_h) $(gxdevice_h) $(gxdevmem_h)

gxcolor.$(OBJ): gxcolor.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxdevice_h) $(gxlum_h) $(gzcolor_h) $(gzht_h) $(gzstate_h)

gxcpath.$(OBJ): gxcpath.c $(GXERR) \
  $(gxdevice_h) $(gxfixed_h) $(gzcolor_h) $(gzpath_h) $(gxcpath_h)

gxdither.$(OBJ): gxdither.c $(GX) \
  $(gxfixed_h) $(gxlum_h) $(gxmatrix_h) $(gzstate_h) $(gzdevice_h) $(gzcolor_h) $(gzht_h)

gxdraw.$(OBJ): gxdraw.c $(GX) \
  $(gxfixed_h) $(gxmatrix_h) $(gxbitmap_h) $(gzcolor_h) $(gzdevice_h) $(gzstate_h)

gxfill.$(OBJ): gxfill.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxdevice_h) $(gzcolor_h) $(gzpath_h) $(gzstate_h) $(gxcpath_h)

gxht.$(OBJ): gxht.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxbitmap_h) $(gzstate_h) $(gzcolor_h) $(gzdevice_h) $(gzht_h)

gxpath.$(OBJ): gxpath.c $(GXERR) \
  $(gxfixed_h) $(gzpath_h)

gxpath2.$(OBJ): gxpath2.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gzpath_h)

gxstroke.$(OBJ): gxstroke.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) $(gzstate_h) $(gzcolor_h) $(gzdevice_h) $(gzline_h) $(gzpath_h)

###### High-level facilities

gschar.$(OBJ): gschar.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) $(gzdevice_h) $(gxdevmem_h) $(gxfont_h) $(gxchar_h) $(gxcache_h) $(gstype1_h) $(gspath_h) $(gzpath_h) $(gzcolor_h) $(gzstate_h)

gscolor.$(OBJ): gscolor.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxdevice_h) $(gzstate_h) $(gzcolor_h) $(gzht_h)

gscoord.$(OBJ): gscoord.c $(GXERR) \
  $(gxarith_h) $(gxfixed_h) $(gxmatrix_h) $(gzdevice_h) $(gzstate_h) $(gscoord_h)

gsdevice.$(OBJ): gsdevice.c $(GXERR) \
  $(gsarith_h) $(gsprops_h) $(gsutil_h) $(gxfixed_h) $(gxmatrix_h) $(gxbitmap_h) $(gxdevmem_h) $(gzstate_h) $(gzdevice_h)

gsfile.$(OBJ): gsfile.c $(GXERR) \
  $(gsmatrix_h) $(gxdevice_h) $(gxdevmem_h)

gsfont.$(OBJ): gsfont.c $(GXERR) \
  $(gxdevice_h) $(gxfixed_h) $(gxmatrix_h) $(gxfont_h) $(gxfdir_h) $(gzstate_h)

gsimage.$(OBJ): gsimage.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) $(gspaint_h) $(gzcolor_h) $(gzdevice_h) $(gzpath_h) $(gzstate_h) $(gxcpath_h) $(gxdevmem_h) $(gximage_h)

gsim2out.$(OBJ): gsim2out.c $(GXERR) \
  $(gsstate_h) $(gsmatrix_h) $(gscoord_h) $(gxfixed_h) $(gxtype1_h)

gsline.$(OBJ): gsline.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gzstate_h) $(gzline_h)

gsmatrix.$(OBJ): gsmatrix.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h)

gsmisc.$(OBJ): gsmisc.c $(GX) $(MAKEFILE)
	$(CCC) -DUSE_ASM=0$(USE_ASM) gsmisc.c

gspaint.$(OBJ): gspaint.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gspaint_h) $(gzpath_h) $(gzstate_h) $(gzdevice_h) $(gxcpath_h) $(gxdevmem_h) $(gximage_h)

gspath.$(OBJ): gspath.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxpath_h) $(gzstate_h)

gspath2.$(OBJ): gspath2.c $(GXERR) \
  $(gspath_h) $(gxfixed_h) $(gxmatrix_h) $(gzstate_h) $(gzpath_h) $(gzdevice_h)

gsstate.$(OBJ): gsstate.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gzstate_h) $(gzcolor_h) $(gzdevice_h) $(gzht_h) $(gzline_h) $(gzpath_h)

gstdev.$(OBJ): gstdev.c $(GX) \
  $(gxbitmap_h) $(gxdevice_h) $(gxfixed_h) $(gxmatrix_h)

gstype1.$(OBJ): gstype1.c $(GXERR) \
  $(gxarith_h) $(gxfixed_h) $(gxmatrix_h) $(gxchar_h) $(gxdevmem_h) $(gxop1_h) $(gxtype1_h) \
  $(gzstate_h) $(gzdevice_h) $(gzpath_h)

###### The internal devices

gdevmem_h=gdevmem.h

gdevmem1.$(OBJ): gdevmem1.c $(AK) \
  $(gs_h) $(gxdevice_h) $(gxdevmem_h) $(gdevmem_h)

gdevmem2.$(OBJ): gdevmem2.c $(AK) \
  $(gs_h) $(gxdevice_h) $(gxdevmem_h) $(gdevmem_h)

gdevmem3.$(OBJ): gdevmem3.c $(AK) \
  $(gs_h) $(gxdevice_h) $(gxdevmem_h) $(gdevmem_h)

###### Files dependent on the installed devices, features, and platform.
###### Generating gconfig.h also generates obj*.tr and lib.tr.

gconfig.h obj.tr objw.tr lib.tr: devs.mak $(MAKEFILE) $(UNIQ) \
  $(DEVICE_DEVS) $(DEVICE_DEVS2) $(DEVICE_DEVS3) $(DEVICE_DEVS4) $(DEVICE_DEVS5)\
  $(FEATURE_DEVS) $(PLATFORM).dev
	$(SHP)gsconfig $(DEVICE_DEVS) +
	$(SHP)gsconfig + $(DEVICE_DEVS2) +
	$(SHP)gsconfig + $(DEVICE_DEVS3) +
	$(SHP)gsconfig + $(DEVICE_DEVS4) +
	$(SHP)gsconfig + $(DEVICE_DEVS5) +
	$(SHP)gsconfig + $(FEATURE_DEVS) $(PLATFORM).dev

gconfig.$(OBJ): gconfig.c $(AK) gconfig.h $(MAKEFILE)
	$(CC0) -DGS_LIB_DEFAULT=$(QQ)$(GS_LIB_DEFAULT)$(QQ) -DGS_INIT=$(QQ)$(GS_INIT)$(QQ) gconfig.c

###### On Unix, we pre-link all of the library except the back end.
###### On MS-DOS, we have to do the whole thing at once.

LIB=gschar.$(OBJ) gscolor.$(OBJ) gscoord.$(OBJ) \
 gsdevice.$(OBJ) gsfile.$(OBJ) gsfont.$(OBJ) \
 gsimage.$(OBJ) gsim2out.$(OBJ) \
 gsline.$(OBJ) gsmatrix.$(OBJ) gsmisc.$(OBJ) \
 gspaint.$(OBJ) gspath.$(OBJ) gspath2.$(OBJ) \
 gsstate.$(OBJ) gstdev.$(OBJ) gstype1.$(OBJ) gsutil.$(OBJ) \
 gxcache.$(OBJ) gxclist.$(OBJ) gxcolor.$(OBJ) gxcpath.$(OBJ) \
 gxdither.$(OBJ) gxdraw.$(OBJ) gxfill.$(OBJ) \
 gxht.$(OBJ) gxpath.$(OBJ) gxpath2.$(OBJ) gxstroke.$(OBJ) \
 gdevmem1.$(OBJ) gdevmem2.$(OBJ) gdevmem3.$(OBJ) gconfig.$(OBJ)

# ------------------------------ Interpreter ------------------------------ #

###### Include files

alloc_h=alloc.h
astate_h=astate.h
ccfont_h=ccfont.h
dict_h=dict.h
dstack_h=dstack.h
errors_h=errors.h
estack_h=estack.h
file_h=file.h
font_h=font.h
ghost_h=ghost.h $(gx_h)
gp_h=gp.h
iutil_h=iutil.h
name_h=name.h
opdef_h=opdef.h
ostack_h=ostack.h
overlay_h=overlay.h
packed_h=packed.h
save_h=save.h
scanchar_h=scanchar.h
state_h=state.h
store_h=store.h
stream_h=stream.h
# Nested include files
oper_h=oper.h $(gsutil_h) $(iutil_h) $(opdef_h) $(ostack_h)
# Include files for optional features
bnum_h=bnum.h
bseq_h=bseq.h
btoken_h=btoken.h

comp1_h=comp1.h $(ghost_h) $(oper_h) $(gserrors_h) $(gxfixed_h) $(gxop1_h)

gdevpccm_h=gdevpccm.h
gdevpcl_h=gdevpcl.h
gdevprn_h=gdevprn.h $(memory__h) $(string__h) $(gs_h) \
  $(gsmatrix_h) $(gxdevice_h) $(gxdevmem_h) $(gxclist_h)
gdevx_h=gdevx.h

###### Utilities

GH=$(AK) $(ghost_h)

ialloc.$(OBJ): ialloc.c $(AK) $(gs_h) $(alloc_h) $(astate_h)

iccfont.$(OBJ): iccfont.c $(GH) \
 $(ghost_h) $(alloc_h) $(ccfont_h) $(dict_h) $(dstack_h) $(errors_h) $(name_h) $(save_h) $(store_h)

idebug.$(OBJ): idebug.c $(GH) \
 $(iutil_h) $(dict_h) $(name_h) $(ostack_h) $(opdef_h) $(packed_h) $(store_h)

idict.$(OBJ): idict.c $(GH) $(alloc_h) $(errors_h) $(name_h) $(packed_h) $(save_h) $(store_h) $(iutil_h) $(dict_h) $(dstack_h)

iinit.$(OBJ): iinit.c $(GH) gconfig.h $(alloc_h) $(dict_h) $(dstack_h) $(errors_h) $(name_h) $(oper_h) $(store_h)

iname.$(OBJ): iname.c $(GH) $(alloc_h) $(errors_h) $(name_h) $(store_h)

isave.$(OBJ): isave.c $(GH) $(alloc_h) $(astate_h) $(name_h) $(packed_h) $(save_h) $(store_h)

iscan.$(OBJ): iscan.c $(GH) $(alloc_h) $(dict_h) $(dstack_h) $(errors_h) $(iutil_h) \
 $(name_h) $(ostack_h) $(packed_h) $(store_h) $(stream_h) $(scanchar_h)

iutil.$(OBJ): iutil.c $(GH) \
 $(errors_h) $(alloc_h) $(dict_h) $(iutil_h) $(name_h) $(ostack_h) $(opdef_h) $(store_h) \
 $(gsmatrix_h) $(gxdevice_h) $(gzcolor_h)

sfilter.$(OBJ): sfilter.c $(AK) $(std_h) $(scanchar_h) $(stream_h) \
 $(gxfixed_h) $(gstype1_h)

stream.$(OBJ): stream.c $(AK) $(std_h) $(stream_h) $(scanchar_h)

###### Operators

OP=$(GH) $(errors_h) $(oper_h)

### Non-graphics operators

zarith.$(OBJ): zarith.c $(OP) $(store_h)

zarray.$(OBJ): zarray.c $(OP) $(alloc_h) $(packed_h) $(store_h)

zcontrol.$(OBJ): zcontrol.c $(OP) $(estack_h) $(iutil_h) $(store_h)

zdict.$(OBJ): zdict.c $(OP) $(dict_h) $(dstack_h) $(store_h)

zfile.$(OBJ): zfile.c $(OP) $(gp_h) \
  $(alloc_h) $(estack_h) $(file_h) $(iutil_h) $(save_h) $(stream_h) $(store_h)

zfileio.$(OBJ): zfileio.c $(OP) $(gp_h) \
  $(estack_h) $(file_h) $(store_h) $(stream_h) \
  $(gsmatrix_h) $(gxdevice_h) $(gxdevmem_h)

zfilter.$(OBJ): zfilter.c $(OP) $(alloc_h) $(stream_h)

zgeneric.$(OBJ): zgeneric.c $(OP) $(dict_h) $(estack_h) $(name_h) $(packed_h) $(store_h)

zmath.$(OBJ): zmath.c $(OP) $(store_h)

zmisc.$(OBJ): zmisc.c $(OP) $(gp_h) \
  $(alloc_h) $(dict_h) $(dstack_h) $(name_h) $(packed_h) $(store_h) \
  $(gstype1_h) $(gxfixed_h)

zpacked.$(OBJ): zpacked.c $(OP) \
  $(alloc_h) $(dict_h) $(name_h) $(packed_h) $(save_h) $(store_h)

zprops.$(OBJ): zprops.c $(OP) \
  $(alloc_h) $(dict_h) $(name_h) $(store_h) \
  $(gsprops_h) $(gsmatrix_h) $(gxdevice_h)

zrelbit.$(OBJ): zrelbit.c $(OP) $(store_h) $(dict_h)

zstack.$(OBJ): zstack.c $(OP) $(store_h)

zstring.$(OBJ): zstring.c $(OP) $(alloc_h) $(iutil_h) $(name_h) $(store_h) $(stream_h)

ztype.$(OBJ): ztype.c $(OP) $(dict_h) $(iutil_h) $(name_h) $(stream_h) $(store_h)

zvmem.$(OBJ): zvmem.c $(OP) $(alloc_h) $(dict_h) $(dstack_h) $(estack_h) $(save_h) $(state_h) $(store_h) \
  $(gsmatrix_h) $(gsstate_h)

###### Graphics operators

zchar.$(OBJ): zchar.c $(OP) $(gxmatrix_h) $(gschar_h) $(gstype1_h) $(gxdevice_h) $(gxfixed_h) $(gxfont_h) $(gzpath_h) $(gzstate_h) $(alloc_h) $(dict_h) $(dstack_h) $(font_h) $(estack_h) $(state_h) $(store_h)

zcolor.$(OBJ): zcolor.c $(OP) $(alloc_h) $(estack_h) $(gxfixed_h) $(gxmatrix_h) $(gzstate_h) $(gxdevice_h) $(gzcolor_h) $(iutil_h) $(state_h) $(store_h)

zdevice.$(OBJ): zdevice.c $(OP) $(alloc_h) $(state_h) $(gsmatrix_h) $(gsstate_h) $(gxdevice_h) $(store_h)

zfont.$(OBJ): zfont.c $(OP) $(gsmatrix_h) $(gxdevice_h) $(gxfont_h) $(gxfdir_h) \
 $(alloc_h) $(font_h) $(dict_h) $(name_h) $(packed_h) $(save_h) $(state_h) $(store_h)

zfont1.$(OBJ): zfont1.c $(OP) $(gsmatrix_h) $(gxdevice_h) $(gschar_h) $(gxfixed_h) $(gxfont_h) \
 $(dict_h) $(font_h) $(name_h) $(store_h)

zfont2.$(OBJ): zfont2.c $(OP) $(gsmatrix_h) $(gxdevice_h) $(gschar_h) $(gxfixed_h) $(gxfont_h) \
 $(alloc_h) $(dict_h) $(font_h) $(name_h) $(packed_h) $(store_h)

zgstate.$(OBJ): zgstate.c $(OP) $(alloc_h) $(gsmatrix_h) $(gsstate_h) $(state_h) $(store_h)

zht.$(OBJ): zht.c $(OP) $(alloc_h) $(estack_h) $(gsmatrix_h) $(gsstate_h) $(state_h) $(store_h)

zmatrix.$(OBJ): zmatrix.c $(OP) $(gsmatrix_h) $(state_h) $(gscoord_h) $(store_h)

zpaint.$(OBJ): zpaint.c $(OP) $(alloc_h) $(estack_h) $(gsmatrix_h) $(gspaint_h) $(state_h) $(store_h)

zpath.$(OBJ): zpath.c $(OP) $(gsmatrix_h) $(gspath_h) $(state_h) $(store_h)

zpath2.$(OBJ): zpath2.c $(OP) $(alloc_h) $(estack_h) $(gspath_h) $(state_h) $(store_h)

###### Linking

INT=ialloc.$(OBJ) idebug.$(OBJ) idict.$(OBJ) iinit.$(OBJ) iname.$(OBJ) \
 interp.$(OBJ) isave.$(OBJ) iscan.$(OBJ) iutil.$(OBJ) \
 sfilter.$(OBJ) stream.$(OBJ) \
 zarith.$(OBJ) zarray.$(OBJ) zcontrol.$(OBJ) zdict.$(OBJ) \
 zfile.$(OBJ) zfileio.$(OBJ) zfilter.$(OBJ) zgeneric.$(OBJ) \
 zmath.$(OBJ) zmisc.$(OBJ) zpacked.$(OBJ) zprops.$(OBJ) zrelbit.$(OBJ) \
 zstack.$(OBJ) zstring.$(OBJ) ztype.$(OBJ) zvmem.$(OBJ) \
 zchar.$(OBJ) zcolor.$(OBJ) zfont.$(OBJ) zfont1.$(OBJ) zfont2.$(OBJ) \
 zdevice.$(OBJ) zgstate.$(OBJ) zht.$(OBJ) zmatrix.$(OBJ) \
 zpaint.$(OBJ) zpath.$(OBJ) zpath2.$(OBJ)

# -------------------------- Optional features ---------------------------- #

### Additions common to Display PostScript and Level 2

dpsand2_=gsdps1.$(OBJ) ibnum.$(OBJ) ibscan.$(OBJ) \
 zbseq.$(OBJ) zdps1.$(OBJ) zupath.$(OBJ)
dpsand2.dev: $(dpsand2_)
	$(SHP)gssetmod dpsand2 $(dpsand2_)
	$(SHP)gsaddmod dpsand2 -oper zbseq zdps1 zupath
	$(SHP)gsaddmod dpsand2 -ps gs_dps1

gsdps1.$(OBJ): gsdps1.c $(GXERR) $(gsmatrix_h) $(gspath_h)

ibnum.$(OBJ): ibnum.c $(GH) $(errors_h) $(stream_h) $(bnum_h) $(btoken_h)

ibscan.$(OBJ): ibscan.c $(GH) $(errors_h) \
 $(alloc_h) $(dict_h) $(dstack_h) $(iutil_h) $(name_h) $(ostack_h) $(save_h) $(store_h) $(stream_h) $(bseq_h) $(btoken_h) $(bnum_h)

zbseq.$(OBJ): zbseq.c $(OP) $(save_h) $(store_h) $(stream_h) $(file_h) $(name_h) $(bnum_h) $(btoken_h) $(bseq_h)

zdps1.$(OBJ): zdps1.c $(OP) $(gsmatrix_h) $(gspath_h) $(gsstate_h) \
 $(state_h) $(store_h) $(stream_h) $(bnum_h)

zupath.$(OBJ): zupath.c $(OP) \
 $(dict_h) $(dstack_h) $(iutil_h) $(state_h) $(store_h) $(stream_h) $(bnum_h) \
 $(gscoord_h) $(gsmatrix_h) $(gspaint_h) $(gspath_h) $(gsstate_h) \
 $(gxfixed_h) $(gxdevice_h) $(gxpath_h)

### Display PostScript

# We should include zcontext, but it isn't in good enough shape yet.
dps_=
dps.dev: dpsand2.dev $(dps_)
	$(SHP)gssetmod dps $(dps_)
	$(SHP)gsaddmod dps -include dpsand2

zcontext.$(OBJ): zcontext.c $(OP) \
 $(alloc_h) $(dict_h) $(estack_h) $(state_h) $(store_h)

### Level 2 additions -- currently just things common to DPS and Level 2.
# Should also include
#	$(SHP)gsaddmod level2 -ps gs_lev2

level2_=
level2.dev: dpsand2.dev $(level2_)
	$(SHP)gssetmod level2 $(level2_)
	$(SHP)gsaddmod level2 -include dpsand2

### Composite font support

gschar0.$(OBJ): gschar0.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gzdevice_h) $(gxdevmem_h) $(gxfont_h) $(gxchar_h) $(gzstate_h)

zfont0.$(OBJ): zfont0.c $(OP) $(gsmatrix_h) $(gxdevice_h) $(gxfont_h) \
 $(alloc_h) $(font_h) $(dict_h) $(name_h) $(state_h) $(store_h)

compfont_=zfont0.$(OBJ) gschar0.$(OBJ)
compfont.dev: $(compfont_)
	$(SHP)gssetmod compfont $(compfont_)
	$(SHP)gsaddmod compfont -oper zfont0

### Filters other than eexec and ASCIIHex

sfilter2.$(OBJ): sfilter2.c $(AK) $(std_h) $(scanchar_h) $(stream_h)

zfilter2.$(OBJ): zfilter2.c $(OP) $(alloc_h) $(stream_h)

filter_=zfilter2.$(OBJ) sfilter2.$(OBJ)
filter.dev: $(filter_)
	$(SHP)gssetmod filter $(filter_)
	$(SHP)gsaddmod filter -oper zfilter2

### LZW encoding/decoding
### Not included, because Unisys has been awarded a legal monopoly
### (patent) on the right to implement the freely published algorithms.

lzw_=zlzwd.$(OBJ) zlzwe.$(OBJ) slzwd.$(OBJ) slzwe.$(OBJ)
lzw.dev: $(lzw_)
	$(SHP)gssetmod lzw $(lzw_)
	$(SHP)gsaddmod lzw -oper zlzwd zlzwe

slzwd.$(OBJ): slzwd.c $(AK) std.h stream.h

slzwe.$(OBJ): slzwe.c $(AK) std.h stream.h

zlzwd.$(OBJ): zlzwd.c $(OP) $(alloc_h) $(stream_h)

zlzwe.$(OBJ): zlzwe.c $(OP) $(alloc_h) $(stream_h)

### Precompiled fonts.  See fonts.doc for more information.

CCFONT=$(OP) $(ccfont_h)

ccfonts_=ugly.$(OBJ) cour.$(OBJ)
ccfonts.dev: $(ccfonts_) iccfont.$(OBJ)
	$(SHP)gssetmod ccfonts $(ccfonts_) iccfont.$(OBJ)
	$(SHP)gsaddmod ccfonts -oper font_Ugly font_Courier

ugly.$(OBJ): ugly.c $(CCFONT)

cour.$(OBJ): cour.c $(CCFONT)

psyr.$(OBJ): psyr.c $(CCFONT)

pzdr.$(OBJ): pzdr.c $(CCFONT)

# ----------------------------- Main program ------------------------------ #

# Utilities shared between platforms

gsmain.$(OBJ): gsmain.c $(GX) \
  $(gp_h) $(gsmatrix_h) $(gxdevice_h)

# Interpreter main program

interp.$(OBJ): interp.c $(GH) \
  $(errors_h) $(estack_h) $(name_h) $(dict_h) $(dstack_h) $(oper_h) $(ostack_h) $(packed_h) $(save_h) $(store_h) $(stream_h)
	$(CCINT) interp.c

gs.$(OBJ): gs.c $(GH) $(alloc_h) $(estack_h) $(ostack_h) $(store_h) $(stream_h)
#    Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# makefile for Ghostscript device drivers.

# -------------------------------- Catalog ------------------------------- #

# It is possible to build Ghostscript with an arbitrary collection of
# device drivers, although some drivers are supported only on a subset
# of the target platforms.  The currently available drivers are:

# Displays:
#   MS-DOS EGA and VGA:
#	ega	EGA (640x350, 16-color)
#	vga	VGA (640x480, 16-color)
#   MS-DOS SuperVGA:
# +	atiw	ATI Wonder SuperVGA, 256-color modes
# *	mdb10	EIZO MDB-10 (1024 x 768)
#	tseng	SuperVGA using Tseng Labs ET3000/4000 chips, 256-color modes
#	tseng16  Tseng Labs SuperVGA in 800x600, 16-color mode (256K memory)
# +	tvga16	Trident SuperVGA in 800x600, 16-color mode (256K memory)
#   ****** NOTE: the vesa device does not work with the Watcom (32-bit MS-DOS)
#   ****** compiler or executable.
#	vesa	SuperVGA with VESA standard API driver
#   MS-DOS other:
#	bgi	Borland Graphics Interface (CGA and Hercules)
# *	pe	Private Eye
#   Unix and VMS:
# *	sonyfb	Sony Microsystems monochrome display   [Sony only]
# *	sunview  SunView window system   [SunOS only]
#	x11	X Windows version 11, release >=3   [Unix and VMS only]
# Printers:
#	bj10e	Canon BubbleJet BJ10e
# *	cdeskjet  H-P DeskJet 500C
# +	deskjet  H-P DeskJet and DeskJet Plus
# *	dfaxhigh  DigiBoard, Inc.'s DigiFAX software format (high resolution)
# *	dfaxlow  DigiFAX low (normal) resolution
#	djet500  H-P DeskJet 500
# *	djet500c  H-P DeskJet 500C
#	epson	Epson-compatible dot matrix printers (9- or 24-pin)
# *	epsonc	Epson LQ-2550 and Fujitsu 3400/2400/1200 color printers
# +	laserjet  H-P LaserJet
# *	lbp8	Canon LBP-8II laser printer
# *	ln03	DEC LN03 printer   [Ultrix only?]
# +	ljet2p	H-P LaserJet IId/IIp/III* with TIFF compression
# +	ljet3	H-P LaserJet III* with Delta Row compression
# *	ljetplus  H-P LaserJet Plus
# *	nwp533  Sony Microsystems NWP533 laser printer   [Sony only]
#	paintjet  H-P PaintJet color printer
# *	r4081	Ricoh 4081 laser printer
# *	trufax	TruFax facsimile driver  [Unix only]
# File formats and others:
#	bit	A "bit bucket" device for time benchmarking
#	gifmono	Monochrome GIF file format
#	gif8	8-bit color GIF file format
#	pcxmono	Monochrome PCX file format
#	pcx16	Older color PCX file format (EGA/VGA, 16-color)
#	pcx256	Newer color PCX file format (256-color)
#	pbm	Portable Bitmap (plain format)
#	pbmraw	Portable Bitmap (raw format)
#	pgm	Portable Graymap (plain format)
#	pgmraw	Portable Graymap (raw format)
#	ppm	Portable Pixmap (plain format)
#	ppmraw	Portable Pixmap (raw format)

# User-contributed drivers marked with * require hardware or software
# that is not available to Aladdin Enterprises.  Please contact the
# original contributors, not Aladdin Enterprises, if you have questions.
# Contact information appears in the driver entry below.
#
# Drivers marked with a + are maintained by Aladdin Enterprises with
# the assistance of users, since Aladdin Enterprises doesn't have access to
# the hardware for these either.

# If you add drivers, it would be nice if you kept each list
# in alphabetical order.

# Each platform-specific makefile contains a line of the form
#	DEVICE_DEVS=<dev1>.dev ... <devn>.dev
# where dev1 ... devn are the devices to be included in the build.
# You may edit this line to select any desired set of devices.
# dev1 will be used as the default device (unless overridden from
# the command line with -sDEVICE=xxx, of course.)  If you can't fit all the
# devices on a single line, you may add lines defining
#	DEVICE_DEVS2=<dev21>.dev ... <dev2n>.dev
#	DEVICE_DEVS3=<dev31>.dev ... <dev3n>.dev
# etc. up to DEVICE_DEVS5.
# Don't use continuation lines, since this may break the MS-DOS command
# processor.

# ---------------------------- End of catalog ---------------------------- #

# If you want to add a new device driver, the examples below should be
# enough of a guide to the correct form for the makefile rules.

# All device drivers depend on the following:
GDEV=$(AK) $(gx_h) $(gxdevice_h)

###### ------------------- MS-DOS display devices ------------------- ######

# There are really only two drivers: an EGA/VGA driver (4 bit-planes,
# plane-addressed) and a SuperVGA driver (8 bit-planes, byte addressed).

### ----------------------- EGA and VGA displays ----------------------- ###

gdevegaa.$(OBJ): gdevegaa.asm

ETEST=ega.$(OBJ) $(ega_)
ega.exe: $(ETEST) libc$(MM).tr
	tlink /m /l $(LIBDIR)\c0$(MM) @ega.tr @libc$(MM).tr

ega.$(OBJ): ega.c $(GDEV)

# The shared MS-DOS makefile defines PCFBASM as either gdevegaa.$(OBJ)
# or an empty string.

EGAVGA=gdevpcfb.$(OBJ) $(PCFBASM)

gdevpcfb.$(OBJ): gdevpcfb.c $(GDEV) $(MAKEFILE) $(dos__h)
	$(CCC) -DUSE_ASM=0$(USE_ASM) gdevpcfb.c

# The EGA/VGA family includes: EGA, VGA, MDB-10, and the
# Tseng ET3000/4000 and Trident SuperVGA in 16-color mode.

ega.dev: $(EGAVGA)
	$(SHP)gssetdev ega $(EGAVGA)

vga.dev: $(EGAVGA)
	$(SHP)gssetdev vga $(EGAVGA)

mdb10.dev: $(EGAVGA)
	$(SHP)gssetdev mdb10 $(EGAVGA)

tseng16.dev: $(EGAVGA)
	$(SHP)gssetdev tseng16 $(EGAVGA)

tvga16.dev: $(EGAVGA)
	$(SHP)gssetdev tvga16 $(EGAVGA)

### ------------------------- SuperVGA displays ------------------------ ###

SVGA=gdevsvga.$(OBJ) $(PCFBASM)

gdevsvga.$(OBJ): gdevsvga.c $(GDEV) $(MAKEFILE) $(dos__h)
	$(CCC) -DUSE_ASM=0$(USE_ASM) gdevsvga.c

# The SuperVGA family includes: ATI Wonder, Tseng ET3000/4000, and VESA.

atiw.dev: $(SVGA)
	$(SHP)gssetdev atiw $(SVGA)

tseng.dev: $(SVGA)
	$(SHP)gssetdev tseng $(SVGA)

vesa.dev: $(SVGA)
	$(SHP)gssetdev vesa $(SVGA)

### ------------ The BGI (Borland Graphics Interface) device ----------- ###

# We should use an implicit rule for running bgiobj,
# but a bug in Borland's `make' utility makes this not work.

cga.$(OBJ): $(BGIDIR)\cga.bgi
	$(BGIDIR)\bgiobj $(BGIDIR)\$&

egavga.$(OBJ): $(BGIDIR)\egavga.bgi
	$(BGIDIR)\bgiobj $(BGIDIR)\$&

herc.$(OBJ): $(BGIDIR)\herc.bgi
	$(BGIDIR)\bgiobj $(BGIDIR)\$&

# Include egavga.$(OBJ) for debugging only.
bgi_=gdevbgi.$(OBJ) cga.$(OBJ) herc.$(OBJ)
bgi.dev: $(bgi_)
	$(SHP)gssetdev bgi $(bgi_)
	$(SHP)gsaddmod bgi -lib $(LIBDIR)\graphics

gdevbgi.$(OBJ): gdevbgi.c $(GDEV) $(MAKEFILE)
	$(CCC) -DBGI_LIB=$(QQ)$(BGIDIR)$(QQ) gdevbgi.c

###### ------------------- The Private Eye display ------------------- ######
### Note: this driver was contributed by a user:                          ###
###   please contact narf@media-lab.media.mit.edu if you have questions.  ###

pe_=gdevpe.$(OBJ)
pe.dev: $(pe_)
	$(SHP)gssetdev pe $(pe_)

gdevpe.$(OBJ): gdevpe.c $(GDEV)

###### --------------- Memory-buffered printer devices --------------- ######

PDEVH=$(GDEV) $(gxdevmem_h) $(gxclist_h) $(gdevprn_h)

gdevprn.$(OBJ): gdevprn.c $(PDEVH) $(gp_h) $(gsprops_h)

### ----------------- The Canon BubbleJet BJ10e device ----------------- ###

bj10e_=gdevbj10.$(OBJ) gdevprn.$(OBJ)
bj10e.dev: $(bj10e_)
	$(SHP)gssetdev bj10e $(bj10e_)

gdevbj10.$(OBJ): gdevbj10.c $(PDEVH)

### -------------------------- The DigiFAX device ----------------------- ###
###    This driver outputs images in a format suitable for use with       ###
###    DigiBoard, Inc.'s DigiFAX software.  Use -sDEVICE=dfaxhigh for     ###
###    high resolution output, -sDEVICE=dfaxlow for normal output.        ###
### Note: this driver was contributed by a user: please contact           ###
###       Rick Richardson (rick@digibd.com) if you have questions.        ###

digifax_=gdevdfax.$(OBJ) gdevprn.$(OBJ)
dfaxhigh.dev: $(digifax_)
	$(SHP)gssetdev dfaxhigh $(digifax_)

dfaxlow.dev: $(digifax_)
	$(SHP)gssetdev dfaxlow $(digifax_)

gdevdfax.$(OBJ): gdevdfax.c $(GDEV) $(gdevprn_h) gdevdfg3.h

### ----------- The H-P DeskJet and LaserJet printer devices ----------- ###

### These are essentially the same device.

HPPCL=gdevprn.$(OBJ) gdevpcl.$(OBJ)
HPMONO=gdevdjet.$(OBJ) $(HPPCL)

gdevpcl.$(OBJ): gdevpcl.c $(PDEVH) $(gdevpcl_h)

gdevdjet.$(OBJ): gdevdjet.c $(PDEVH) $(gdevpcl_h)

deskjet.dev: $(HPMONO)
	$(SHP)gssetdev deskjet $(HPMONO)

djet500.dev: $(HPMONO)
	$(SHP)gssetdev djet500 $(HPMONO)

laserjet.dev: $(HPMONO)
	$(SHP)gssetdev laserjet $(HPMONO)

ljetplus.dev: $(HPMONO)
	$(SHP)gssetdev ljetplus $(HPMONO)

### Selecting ljet2p provides TIFF (mode 2) compression on LaserJet III,
### IIIp, IIId, IIIsi, IId, and IIp. 

ljet2p.dev: $(HPMONO)
	$(SHP)gssetdev ljet2p $(HPMONO)

### Selecting ljet3 provides Delta Row (mode 3) compression on LaserJet III,
### IIIp, IIId, IIIsi.

ljet3.dev: $(HPMONO)
	$(SHP)gssetdev ljet3 $(HPMONO)

### ------------ The H-P DeskJet 500C color printer device -------------- ###
### Note: there are two different 500C drivers, both contributed by users.###
###   If you have questions about the djet500c driver,                    ###
###       please contact AKayser@et.tudelft.nl.                           ###
###   If you have questions about the cdeskjet driver,                    ###
###       please contact g.cameron@aberdeen.ac.uk.                        ###

cdeskjet_=gdevcdj.$(OBJ) $(HPPCL)
cdeskjet.dev: $(cdeskjet_)
	$(SHP)gssetdev cdeskjet $(cdeskjet_)

gdevcdj.$(OBJ): gdevcdj.c $(PDEVH) $(gdevpcl_h)

djet500c_=gdevdjtc.$(OBJ) $(HPPCL)
djet500c.dev: $(djet500c_)
	$(SHP)gssetdev djet500c $(djet500c_)

gdevdjtc.$(OBJ): gdevdjtc.c $(PDEVH) $(gdevpcl_h)

### ----------------- The generic Epson printer device ----------------- ###

epson_=gdevepsn.$(OBJ) gdevprn.$(OBJ)
epson.dev: $(epson_)
	$(SHP)gssetdev epson $(epson_)

gdevepsn.$(OBJ): gdevepsn.c $(PDEVH) devs.mak

### -------------- The Epson LQ-2550 color printer device -------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Dave St. Clair (dave@exlog.com) if you have questions.         ###

epsonc_=gdevepsc.$(OBJ) gdevprn.$(OBJ)
epsonc.dev: $(epsonc_)
	$(SHP)gssetdev epsonc $(epsonc_)

gdevepsc.$(OBJ): gdevepsc.c $(PDEVH) devs.mak

### ------------ The H-P PaintJet color printer device ----------------- ###

paintjet_=gdevpjet.$(OBJ) $(HPPCL)
paintjet.dev: $(paintjet_)
	$(SHP)gssetdev paintjet $(paintjet_)

gdevpjet.$(OBJ): gdevpjet.c $(PDEVH) $(gdevpcl_h)

### ----------------- The Canon LBP-8II printer device ----------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Tom Quinn (trq@prg.oxford.ac.uk) if you have questions.        ###
### Note that the standard paper size for this driver is the European    ###
###   A4 size, not the American 8.5" x 11" size.                         ###

lbp8_=gdevlbp8.$(OBJ) gdevprn.$(OBJ)
lbp8.dev: $(lbp8_)
	$(SHP)gssetdev lbp8 $(lbp8_)

gdevlbp8.$(OBJ): gdevlbp8.c $(PDEVH)

### ----------------- The DEC LN03 printer device ---------------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Ulrich Mueller (ulm@vsnhd1.cern.ch) if you have questions.     ###
### A more general sixel driver is available from                        ###
###       Ian MacPhedran (macphed@dvinci.USask.CA).                      ###

ln03_=gdevln03.$(OBJ) gdevprn.$(OBJ)
ln03.dev: $(ln03_)
	$(SHP)gssetdev ln03 $(ln03_)

gdevln03.$(OBJ): gdevln03.c $(PDEVH)

### ------------- The Ricoh 4081 laser printer device ------------------ ###
### Note: this driver was contributed by users:                          ###
###       please contact kdw@oasis.icl.co.uk if you have questions.      ###

r4081_=gdev4081.$(OBJ) gdevprn.$(OBJ)
r4081.dev: $(r4081_)
	$(SHP)gssetdev r4081 $(r4081_)

gdev4081.$(OBJ): gdev4081.c $(PDEVH)

###### ------------------------ Sony devices ------------------------ ######
### Note: these drivers were contributed by users: please contact        ###
###       Mike Smolenski (mike@intertech.com) if you have questions.     ###

### ------------------- Sony NeWS frame buffer device ------------------ ###

sonyfb_=gdevsnfb.$(OBJ) gdevprn.$(OBJ)
sonyfb.dev: $(sonyfb_)
	$(SHP)gssetdev sonyfb $(sonyfb_)

gdevsnfb.$(OBJ): gdevsnfb.c $(PDEVH)

### -------------------- Sony NWP533 printer device -------------------- ###

nwp533_=gdevn533.$(OBJ) gdevprn.$(OBJ)
nwp533.dev: $(nwp533_)
	$(SHP)gssetdev nwp533 $(nwp533_)

gdevn533.$(OBJ): gdevn533.c $(PDEVH)

###### --------------------- The SunView device --------------------- ######
### Note: this driver is maintained by a user: if you have questions,    ###
###       please contact Andreas Stolcke (stolcke@icsi.berkeley.edu).    ###

sunview_=gdevsun.$(OBJ)
sunview.dev: $(sunview_)
	$(SHP)gssetdev sunview $(sunview_)
	$(SHP)gsaddmod sunview -lib suntool sunwindow pixrect

gdevsun.$(OBJ): gdevsun.c $(GDEV) $(arch_h)

### ----------------- The TruFax facsimile device ---------------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Neil Ostroff (nao@maestro.bellcore.com) if you have questions. ###
### Note that the driver requires a file encode_l.o supplied by the      ###
###   makers of the TruFax product.                                      ###

trufax_=gdevtrfx.$(OBJ) gdevprn.$(OBJ) encode_l.$(OBJ)
trufax.dev: $(trufax_)
	$(SHP)gssetdev trufax $(trufax_)

gdevtrfx.$(OBJ): gdevtrfx.c $(GDEV)

###### ----------------------- The X11 device ----------------------- ######

# Aladdin Enterprises does not support Ghostview.  For more information
# about Ghostview, please contact Tim Theisen (ghostview@cs.wisc.edu).

x11_=gdevx.$(OBJ) gdevxini.$(OBJ)
x11.dev: $(x11_)
	$(SHP)gssetdev x11 $(x11_)
	$(SHP)gsaddmod x11 -lib X11

# See the main makefile for the definition of XINCLUDE.
GDEVX=$(GDEV) x_.h gdevx.h $(MAKEFILE)
gdevx.$(OBJ): gdevx.c $(GDEVX)
	$(CCC) $(XINCLUDE) gdevx.c

gdevxini.$(OBJ): gdevxini.c $(GDEVX)
	$(CCC) $(XINCLUDE) gdevxini.c

### ---------------------- The bit bucket device ----------------------- ###

bit_=gdevbit.$(OBJ) gdevprn.$(OBJ)
bit.dev: $(bit_)
	$(SHP)gssetdev bit $(bit_)

gdevbit.$(OBJ): gdevbit.c $(PDEVH)

###### ----------------------- PC file formats ---------------------- ######

gdevpccm.$(OBJ): gdevpccm.c $(gs_h) $(gsmatrix_h) $(gxdevice_h) $(gdevpccm_h)

### ------------------------- GIF file formats ------------------------- ###

GIF=gdevgif.$(OBJ) gdevpccm.$(OBJ) gdevprn.$(OBJ)

gdevgif.$(OBJ): gdevgif.c $(PDEVH) $(gdevpccm_h)

gifmono.dev: $(GIF)
	$(SHP)gssetdev gifmono $(GIF)

gif8.dev: $(GIF)
	$(SHP)gssetdev gif8 $(GIF)

### ------------------------- PCX file formats ------------------------- ###

PCX=gdevpcx.$(OBJ) gdevpccm.$(OBJ) gdevprn.$(OBJ)

gdevpcx.$(OBJ): gdevpcx.c $(PDEVH) $(gdevpccm_h)

pcxmono.dev: $(PCX)
	$(SHP)gssetdev pcxmono $(PCX)

pcx16.dev: $(PCX)
	$(SHP)gssetdev pcx16 $(PCX)

pcx256.dev: $(PCX)
	$(SHP)gssetdev pcx256 $(PCX)

###### ------------------- Portable Bitmap devices ------------------ ######
### For more information, see the pbm(5), pgm(5), and ppm(5) man pages.  ###

PXM=gdevpbm.$(OBJ) gdevprn.$(OBJ)

gdevpbm.$(OBJ): gdevpbm.c $(PDEVH) $(gxlum_h)

### Portable Bitmap (PBM, plain or raw format, magic numbers "P1" or "P4")

pbm.dev: $(PXM)
	$(SHP)gssetdev pbm $(PXM)

pbmraw.dev: $(PXM)
	$(SHP)gssetdev pbmraw $(PXM)

### Portable Graymap (PGM, plain or raw format, magic numbers "P2" or "P5")

pgm.dev: $(PXM)
	$(SHP)gssetdev pgm $(PXM)

pgmraw.dev: $(PXM)
	$(SHP)gssetdev pgmraw $(PXM)

### Portable Pixmap (PPM, plain or raw format, magic numbers "P3" or "P6")

ppm.dev: $(PXM)
	$(SHP)gssetdev ppm $(PXM)

ppmraw.dev: $(PXM)
	$(SHP)gssetdev ppmraw $(PXM)
#    Copyright (C) 1990, 1992 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Partial makefile for Ghostscript, common to all Unix configurations.

# This is the last part of the makefile for Unix configurations.
# Since Unix make doesn't have an 'include' facility, we concatenate
# the various parts of the makefile together by brute force (in tar_cat).

# The following prevents GNU make from constructing argument lists that
# include all environment variables, which can easily be longer than
# brain-damaged system V allows.

.NOEXPORT:

# -------------------------------- Library -------------------------------- #

## The Unix platform

unix__=gp_unix.$(OBJ)
unix_.dev: $(unix__)
	$(SHP)gssetmod unix_ $(unix__)

gp_unix.$(OBJ): gp_unix.c $(memory__h) $(string__h) $(gx_h) $(gp_h) \
 $(stat__h) $(time__h)

# -------------------------- Auxiliary programs --------------------------- #

ansi2knr$(XE):
	$(CC) -o ansi2knr$(XE) $(CFLAGS) ansi2knr.c

# On the RS/6000 (at least), compiling genarch.c with gcc with -O
# produces a buggy executable.
genarch$(XE): genarch.c
	$(CC) -o genarch$(XE) genarch.c

# ----------------------------- Main program ------------------------------ #

# Main program

ALLUNIX=gsmain.$(OBJ) $(LIB)

# Interpreter main program

GSUNIX=gs.$(OBJ) $(INT) $(ALLUNIX)

gs: $(GSUNIX) obj.tr lib.tr
	$(SHP)echoq $(CC) $(LDFLAGS) $(XLIBDIRS) -o gs $(GSUNIX) >_temp_
	cat obj.tr >>_temp_
	cat lib.tr >>_temp_
	echo $(EXTRALIBS) -lm >>_temp_
	$(SH) <_temp_

# Installation

TAGS:
	etags -t *.c *.h

install: gs
	-mkdir $(bindir)
	$(INSTALL_PROGRAM) gs $(bindir)
	$(INSTALL_PROGRAM) gsnd $(bindir)
	$(INSTALL_PROGRAM) bdftops $(bindir)
	$(INSTALL_PROGRAM) font2c $(bindir)
	$(INSTALL_PROGRAM) pfbtogs $(bindir)
	-mkdir $(libdir)
	$(INSTALL_DATA) gs_init.ps $(libdir)
	$(INSTALL_DATA) gs_2asc.ps $(libdir)
	$(INSTALL_DATA) gs_dps1.ps $(libdir)
	$(INSTALL_DATA) gs_fonts.ps $(libdir)
	$(INSTALL_DATA) gs_lev2.ps $(libdir)
	$(INSTALL_DATA) gs_statd.ps $(libdir)
	$(INSTALL_DATA) sym__enc.ps $(libdir)
	$(INSTALL_DATA) quit.ps $(libdir)
	$(INSTALL_DATA) Fontmap $(libdir)
	$(INSTALL_DATA) uglyr.gsf $(libdir)
	$(INSTALL_DATA) chess.ps $(libdir)
	$(INSTALL_DATA) cheq.ps $(libdir)
	$(INSTALL_DATA) golfer.ps $(libdir)
	$(INSTALL_DATA) escher.ps $(libdir)
	$(INSTALL_DATA) decrypt.ps $(libdir)
	$(INSTALL_DATA) bdftops.ps $(libdir)
	$(INSTALL_DATA) font2c.ps $(libdir)
	$(INSTALL_DATA) pfbtogs.ps $(libdir)
	$(INSTALL_DATA) pstoppm.ps $(libdir)
	$(INSTALL_DATA) prfont.ps $(libdir)
	$(INSTALL_DATA) showpbm.ps $(libdir)
