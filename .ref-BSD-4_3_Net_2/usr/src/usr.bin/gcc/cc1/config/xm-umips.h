/* Configuration for GNU C-compiler for UMIPS operating system
   Copyright (C) 1989 Free Software Foundation, Inc.

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

/*
 *  Notes for compiling gcc on umips (v3.0)
 *	- change the -g in the CFLAGS to a -g3 or take it out all together.
 *	- do not define DBX_DEBUGGING_INFO in tm.h, it doesn't exist (unless
 *	you get one from a bsd system)
 */
#include "xm-mips.h"

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
#define alloca __builtin_alloca
#endif

#define USG

/* for the emacs version of alloca */
#define STACK_DIRECTION	-1

#define bcopy(a,b,c)	memcpy((b),(a),(c))
#define bzero(a,b)	memset((a),0,(b))
#define bcmp(a,b,c)	memcmp((a),(b),(c))
