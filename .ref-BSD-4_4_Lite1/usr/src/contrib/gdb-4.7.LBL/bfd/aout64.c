/* BFD back-end for 64-bit a.out files.
   Copyright 1990, 1991, 1992  Free Software Foundation, Inc.
   Written by Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HOST_64_BIT
#define ARCH_SIZE 64
#include "aoutx.h"
#else
/* Prevent "empty translation unit" warnings from the idiots at X3J11. */
static char ansi_c_idiots = 69;
#endif
