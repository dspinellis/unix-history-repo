/* Macro definitions for GDB on all SVR4 target systems.
   Copyright (C) 1991, 1992, Free Software Foundation, Inc.
   Written by Fred Fish at Cygnus Support (fnf@cygnus.com).

This file is part of GDB.

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

#include "solib.h"	/* Support for shared libraries. */

/* For SVR4 shared libraries, each call to a library routine goes through
   a small piece of trampoline code in the ".init" section.  Although each
   of these fragments is labeled with the name of the routine being called,
   the gdb symbol reading code deliberately ignores them so it won't confuse
   them with the real functions.  It does however know about the label that
   precedes all of the fragments, which is "_init".  Thus when we lookup a
   function that corresponds to a PC value which is in one of the trampoline
   fragments, we'll appear to be in the function "_init".  The following
   macro will evaluate to nonzero when NAME is valid and matches "_init".
   The horribly ugly wait_for_inferior() routine uses this macro to detect
   when we have stepped into one of these fragments. */

#define IN_SOLIB_TRAMPOLINE(pc,name) ((name) && (strcmp ("_init", name) == 0))

/* In SVR4 ELF files, the ABI defines (pg. 4-25) that "External C symbols
   have the same names in C, assembly code, and object files' symbol
   tables."  So... */

#undef	NAMES_HAVE_UNDERSCORE
