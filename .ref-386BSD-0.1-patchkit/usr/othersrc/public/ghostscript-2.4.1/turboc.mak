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

# makefile for Ghostscript, MS-DOS/Turbo C platform.

# ------------------------------- Options ------------------------------- #

###### This section is the only part of the file you should need to edit.

# ------ Generic options ------ #

# Define the default directory/ies for the runtime
# initialization and font files.  Separate multiple directories with \;.
# Use \\ or / to indicate directories, not a single \.

GS_LIB_DEFAULT=c:/gs\;c:/gs/fonts

# Define the name of the Ghostscript initialization file.
# (There is no reason to change this.)

GS_INIT=gs_init.ps

# Choose generic configuration options.

# Setting DEBUG=1 includes debugging features (-Z switch) in the code.
# Code runs substantially slower even if no debugging switches are set,
# and also takes about another 25K of memory.

DEBUG=0

# Setting NOPRIVATE=1 makes private (static) procedures and variables public,
# so they are visible to the debugger and profiler.
# No execution time or space penalty, just larger .OBJ and .EXE files.

NOPRIVATE=0

# ------ Platform-specific options ------ #

# If you don't have an assembler, set USE_ASM=0.  Otherwise, set USE_ASM=1,
# and set ASM to the name of the assembler you are using.  This can be
# a full path name if you want.  Normally it will be masm or tasm.

USE_ASM=1
ASM=masm

# Define the drive and directory for the Turbo C files.
# COMPDIR contains the compiler and linker (normally \tc).
# BGIDIR contains the BGI files (normally /tc).  Note that BGIDIR,
#   unlike all the others, must use / rather than \.
# INCDIR contains the include files (normally \tc\include).
# LIBDIR contains the library files (normally \tc\lib).
# Note that these prefixes are always followed by a \,
#   so if you want to use the current directory, use an explicit '.'.

COMPDIR=c:\tc
BGIDIR=c:/tc
INCDIR=c:\tc\include
LIBDIR=c:\tc\lib

# Choose platform-specific options.

# Define the processor (CPU) type.  Options are 86, 186, 286, 386, or 486.
# (The 8086 and 8088 both correspond to processor type 86.)
# 286 and up do not use protected mode.  Higher numbers produce
# code that may be significantly smaller and faster, but the executable
# will bail out with an error message on lower-numbered processor types.

CPU_TYPE=86

# Define the math coprocessor (FPU) type.  Options are 0, 87, 287, or 387.
# If the CPU type is 486, the FPU type is irrelevant, since the 80486
# CPU includes the equivalent of an 80387 on-chip.
# A non-zero option means that the executable will only run if a FPU
# of that type (or higher) is available: this is NOT currently checked
# at runtime.
#   Code is significantly faster.

FPU_TYPE=0

# ---------------------------- End of options ---------------------------- #

# Define the name of the makefile -- used in dependencies.

MAKEFILE=turboc.mak

# Define the ANSI-to-K&R dependency.  (Turbo C accepts ANSI syntax.)

AK=

# Define the compilation flags for an 80286.

F286=-1

!include "tccommon.mak"

# Define the compilation flags.

!if $(NOPRIVATE)
CP=-DNOPRIVATE
!else
CP=
!endif

!if $(DEBUG)
CD=-DDEBUG
!else
CD=
!endif

GENOPT=$(CP) $(CD)

CCFLAGS=$(GENOPT) $(PLATOPT) $(FPFLAGS) -m$(MM)
CC=$(COMPDIR)\tcc -d -r -y -G -N -I$(INCDIR)
CCC=$(CC) -a $(CCFLAGS) -O -c
CC0=$(COMPDIR)\tcc -m$(MM) -I$(INCDIR) -c
CCINT=$(CC) -a $(CCFLAGS) -c

.c.obj:
	$(CCC) $<

.c.i:
	$(COMPDIR)\cpp -I$(INCDIR) -a $(CCFLAGS) -P- $<

# ------ Devices and features ------ #

# Choose the language feature(s) to include.  See gs.mak for details.
# Note that because Turbo C doesn't use overlays,
# we don't include any optional features.

FEATURE_DEVS=

# Choose the device(s) to include.  See devs.mak for details.
# Note that because Turbo C doesn't use overlays,
# we only include a limited set of device drivers.

DEVICE_DEVS=vga.dev ega.dev epson.dev bj10e.dev
!include "gs.mak"
!include "devs.mak"

# -------------------------------- Library -------------------------------- #

# We have to compile gp_itbc without -1, because it includes a run-time
# check to make sure we are running on the right kind of processor.
gp_itbc.$(OBJ): gp_itbc.c $(string__h) $(gx_h) $(gp_h) $(MAKEFILE) makefile
	$(CC) -a -m$(MM) $(GENOPT) -DCPU_TYPE=$(CPU_TYPE) -c gp_itbc.c

gp_dosfb.$(OBJ): gp_dosfb.c $(memory__h) $(gx_h) $(gp_h) $(gserrors_h) $(gxdevice_h)

gp_msdos.$(OBJ): gp_msdos.c $(dos__h) $(string__h) $(gx_h) $(gp_h)

# ----------------------------- Main program ------------------------------ #

iutilasm.$(OBJ): iutilasm.asm

# A rule to do a quick and dirty compilation attempt when first installing
# Ghostscript.  Many of the compilations will fail: follow this with 'make'.

begin:
	erase arch.h
	erase genarch.exe
	make arch.h
	- $(CCC) *.c
	erase gp_*.obj
	erase gdevepsn.obj

# Get around the fact that the DOS shell has a rather small limit on
# the length of a command line.  (sigh)

LIBCTR=libc$(MM).tr

$(LIBCTR): $(MAKEFILE) makefile
	gsecho $(LIBCTR) $(LIBDIR)\$(FPLIB)+
	gsecho -a $(LIBCTR) $(LIBDIR)\math$(MM) $(LIBDIR)\c$(MM)

LIBDOS=$(LIB) obj.tr

# Interpreter main program

GS_ALL=gs.$(OBJ) $(INT) $(INTASM) gsmain.$(OBJ)\
  $(LIBDOS) $(LIBCTR) obj.tr lib.tr

gs.exe: $(GS_ALL)
	tlink /m /l $(LIBDIR)\c0$(MM) @obj.tr $(INTASM) @gs.tr ,gs,gs,@lib.tr @$(LIBCTR)
