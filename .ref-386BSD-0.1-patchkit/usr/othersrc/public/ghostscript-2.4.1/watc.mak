#    Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

# makefile for Ghostscript, MS-DOS/Watcom C386 platform.

# ------------------------------- Options ------------------------------- #

###### This section is the only part of the file you should need to edit.

# ------ Generic options ------ #

# Define the default directory/ies for the runtime
# initialization and font files.  Separate multiple directories with \;.
# Use / to indicate directories, not a single \.

GS_LIB_DEFAULT=c:/gs\;c:/gs/fonts

# Define the name of the Ghostscript initialization file.
# (There is no reason to change this.)

GS_INIT=gs_init.ps

# Choose generic configuration options.

# Setting DEBUG=1 includes debugging features (-Z switch) in the code.
# Code runs substantially slower even if no debugging switches are set,
# and also takes about another 25K of memory.

DEBUG=0

# Setting TDEBUG=1 includes symbol table information for the Watcom debugger.
# No execution time or space penalty, just larger .OBJ and .EXE files
# and slower linking.

TDEBUG=0

# Setting NOPRIVATE=1 makes private (static) procedures and variables public,
# so they are visible to the debugger and profiler.
# No execution time or space penalty, just larger .OBJ and .EXE files.

NOPRIVATE=0

# ------ Platform-specific options ------ #

# Define the drive, directory, and compiler name for the Watcom C files.
# COMP is the full compiler path name (normally \watcom\bin\wcc386p).
# LINK is the full linker path name (normally \watcom\bin\wlinkp).
# CLINK is the compile-and-link utility full path name (normally
#   \watcom\binb\wcl386).
# STUB is the full path name for the DOS extender stub (normally
#   \watcom\binb\wstub.exe).
# INCDIR contains the include files (normally \watcom\h).
# LIBDIR contains the library files (normally \watcom\lib386).
# Note that INCDIR and LIBDIR are always followed by a \,
#   so if you want to use the current directory, use an explicit '.'.

COMP=c:\watc\bin\wcc386p
LINK=c:\watc\bin\wlinkp
CLINK=c:\watc\binb\wcl386
STUB=c:\watc\binb\wstub.exe
INCDIR=c:\watc\h
LIBDIR=c:\watc\lib386

# Choose platform-specific options.

# Define the processor (CPU) type.  Options are 386 or 486.
# Currently the only difference is that 486 always uses in-line
# floating point.

CPU_TYPE=386

# Define the math coprocessor (FPU) type.  Options are 0, 287, or 387.
# If the CPU type is 486, the FPU type is irrelevant, since the 80486
# CPU includes the equivalent of an 80387 on-chip.
# A non-zero option means that the executable will only run if a FPU
# of that type (or higher) is available: this is NOT currently checked
# at runtime.
#   Code is significantly faster.

FPU_TYPE=0

# ---------------------------- End of options ---------------------------- #

# We want Unix-compatible behavior.  This is part of it.

.NOCHECK

# Define additional extensions to keep `make' happy

.EXTENSIONS: .be .z

# Define the platform name.

PLATFORM=watc_

# Define the name of the makefile -- used in dependencies.

MAKEFILE=watc.mak

# Define the ANSI-to-K&R dependency.  Watcom C accepts ANSI syntax.

AK=

# Define the extensions for the object and executable files.

OBJ=obj
XE=.exe

# Define the need for uniq.

UNIQ=uniq$(XE)

# Define the current directory prefix, shell quote string, and shell name.

EXPP=dos4gw
QQ="
SH=
SHP=

# Define the generic compilation flags.

PLATOPT=

!ifeq CPU_TYPE 486
FPFLAGS=-fpi87
!else
!ifeq FPU_TYPE 387
FPFLAGS=-fpi87
!else
!ifeq FPU_TYPE 287
FPFLAGS=-fpi287
!else
FPFLAGS=
!endif
!endif
!endif

INTASM=
PCFBASM=

# Define the generic compilation rules.

.asm.obj:
	$(ASM) $(ASMFLAGS) $<;

# Make sure we get the right default target for make.

dosdefault: gs$(XE)
	%null

# -------------------------- Auxiliary programs --------------------------- #

genarch$(XE): genarch.c
	$(CCL) $(CCFLAGS) -i=$(LIBDIR) genarch.c

# We need a substitute for the Unix uniq utility.
# It only has to handle stdin and stdout, no options.
uniq$(XE): uniq.c
	echo OPTION STUB=$(STUB) >_temp_.tr
	$(CCL) $(CCFLAGS) -i=$(LIBDIR) @_temp_.tr uniq.c

# Define the compilation flags.

!ifneq NOPRIVATE 0
CP=-dNOPRIVATE
!else
CP=
!endif

!ifneq DEBUG 0
CD=-dDEBUG
!else
CD=
!endif

!ifneq TDEBUG 0
CT=-d2
LCT=DEBUG ALL
!else
CT=-d1
LCT=DEBUG LINES
!endif

GENOPT=$(CP) $(CD) $(CT)

CCFLAGS=$(GENOPT) $(PLATOPT) $(FPFLAGS)
CC=$(COMP) -oi -i=$(INCDIR) $(CCFLAGS)
CCL=$(CLINK) -p -oi -i=$(INCDIR) -l=dos4g
CCC=$(CC)
CC0=$(CC)
CCINT=$(CC)

.c.obj:
	$(CCC) $<

# ------ Devices and features ------ #

# Choose the language feature(s) to include.  See gs.mak for details.
# Since we have a large address space, we include the optional features.

FEATURE_DEVS=filter.dev dps.dev level2.dev

# Choose the device(s) to include.  See devs.mak for details.

DEVICE_DEVS=vga.dev ega.dev tseng.dev epson.dev bj10e.dev paintjet.dev
DEVICE_DEVS2=deskjet.dev djet500.dev laserjet.dev ljetplus.dev ljet2p.dev ljet3.dev
DEVICE_DEVS3=pbm.dev pbmraw.dev pgm.dev pgmraw.dev ppm.dev ppmraw.dev
DEVICE_DEVS4=gifmono.dev gif8.dev pcxmono.dev pcx16.dev pcx256.dev
!include gs.mak
!include devs.mak

# -------------------------------- Library -------------------------------- #

# The Watcom C platform

watc__=gp_iwatc.$(OBJ) gp_dosfb.$(OBJ) gp_msdos.$(OBJ)
watc_.dev: $(watc__)
	$(SHP)gssetmod watc_ $(watc__)

gp_iwatc.$(OBJ): gp_iwatc.c $(string__h) $(gx_h) $(gp_h)

gp_dosfb.$(OBJ): gp_dosfb.c $(memory__h) $(gx_h) $(gp_h) $(gserrors_h) $(gxdevice_h)

gp_msdos.$(OBJ): gp_msdos.c $(dos__h) $(string__h) $(gx_h) $(gp_h)

# ----------------------------- Main program ------------------------------ #

# A rule to do a quick and dirty compilation attempt when first installing
# Ghostscript.  Many of the compilations will fail: follow this with 'make'.

begin:
	erase ccf.tr
	erase arch.h
	erase genarch.exe
	make arch.h
	- $(CCC) *.c
	erase gp_*.obj
	erase gdevepsn.obj

LIBDOS=$(LIB) gp_iwatc.$(OBJ) gp_dosfb.$(OBJ) gp_msdos.$(OBJ) objw.tr lib.tr

# Interpreter main program

GS_ALL=gs.$(OBJ) $(INT) $(INTASM) gsmain.$(OBJ) $(LIBDOS)

gs.exe: $(GS_ALL)
	echo OPTION STUB=$(STUB) >_temp_.tr
	echo LIBRARY $(LIBDIR)\math387r >>_temp_.tr
	echo LIBRARY $(LIBDIR)\DOS\clib3r >>_temp_.tr
	$(LINK) $(LCT) @gsw.tr @objw.tr @_temp_.tr
	erase _temp_.tr
