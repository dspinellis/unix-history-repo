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
