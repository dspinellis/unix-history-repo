/* Configuration for GNU C-compiler for Vax.
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* #defines that need visibility everywhere.  */
#define FALSE 0
#define TRUE 1

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR 8
#define HOST_BITS_PER_SHORT 16
#define HOST_BITS_PER_INT 32
#define HOST_BITS_PER_LONG 32

#define SUCCESS_EXIT_CODE 1
#define FATAL_EXIT_CODE (44 | 0x10000000)  /* Failure, and no DCL message.  */


/* A couple of conditionals for execution machine are controlled here.  */
#ifndef VMS
#define VMS
#endif

#ifndef __GNUC__
/* not present, at least in VAX-11 C (VMS) v2.2 */
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0

#define unlink delete
#endif

/* global const variables don't work,
   so turn off const-ness to prevent trouble with insn-output.c.  */
#define const

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
#define alloca __builtin_alloca
#endif

