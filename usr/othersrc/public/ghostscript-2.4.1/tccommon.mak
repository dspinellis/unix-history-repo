#    Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
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

# Section of MS-DOS makefile for Ghostscript common to
# Turbo C and Turbo/Borland C++.

# Make sure we get the right default target for make.

dosdefault: default

# Define the platform name.

PLATFORM=tbc_

# Define the extensions for the object and executable files.

OBJ=obj
XE=.exe

# Define the need for uniq.

UNIQ=uniq$(XE)

# Define the current directory prefix, shell quote string, and shell name.

EXP=
QQ="
SH=
SHP=

# Define the memory model for Turbo C.  Don't change it!

MM=l

# Define the generic compilation flags.

!if $(CPU_TYPE) >= 486
ASMFLAGS=/DFOR80386 /DFOR80486
PLATOPT=$(F286) -DFOR80386 -DFOR80486
!elif $(CPU_TYPE) >= 386
ASMFLAGS=/DFOR80386
PLATOPT=$(F286) -DFOR80386
!elif $(CPU_TYPE) >= 286
ASMFLAGS=
PLATOPT=$(F286)
!elif $(CPU_TYPE) >= 186
ASMFLAGS=
PLATOPT=-1
!else
ASMFLAGS=
PLATOPT=
!endif

!if $(CPU_TYPE) == 486 || $(FPU_TYPE) >= 287
FPFLAGS=-f287
FPLIB=fp87
!elif $(FPU_TYPE) != 0
FPFLAGS=-f87
FPLIB=fp87
!else
FPFLAGS=
FPLIB=emu
!endif

!if $(USE_ASM)
INTASM=iutilasm.$(OBJ)
PCFBASM=gdevegaa.$(OBJ)
!else
INTASM=
PCFBASM=
!endif

# Define the generic compilation rules.

.asm.obj:
	$(ASM) $(ASMFLAGS) $<;

# -------------------------- Auxiliary programs --------------------------- #

# The dependencies on AK are semi-bogus: AK is null for Turbo C,
# ccf.tr for Turbo/Borland C++.

ansi2knr$(XE): ansi2knr.c $(AK)
	$(CC) $(CCFLAGS) -L$(LIBDIR) ansi2knr.c

genarch$(XE): genarch.c $(AK)
	$(CC) $(CCFLAGS) -L$(LIBDIR) genarch.c

# We need a substitute for the Unix uniq utility.
# It only has to handle stdin and stdout, no options.
uniq$(XE): uniq.c
	$(CC) $(CCFLAGS) -L$(LIBDIR) uniq.c

# -------------------------------- Library -------------------------------- #

# The Turbo/Borland C(++) platform

tbc__=gp_itbc.$(OBJ) gp_dosfb.$(OBJ) gp_msdos.$(OBJ)
tbc_.dev: $(tbc__)
	$(SHP)gssetmod tbc_ $(tbc__)
