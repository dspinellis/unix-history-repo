/* Parameters for execution on VxWorks 68k's, for GDB, the GNU debugger.
   Copyright (C) 1986, 1987, 1989, 1991 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

#define	GDBINIT_FILENAME	".os68gdbinit"

#define	DEFAULT_PROMPT		"(os68k) "

#include "tm-68k.h"

/* We have more complex, useful breakpoints on the target.  */
#undef DECR_PC_AFTER_BREAK
#define	DECR_PC_AFTER_BREAK	0

/* We are guaranteed to have a zero frame pointer at bottom of stack, too. */
#undef	FRAME_CHAIN
#undef	FRAME_CHAIN_VALID

/* Takes the current frame-struct pointer and returns the chain-pointer
   to get to the calling frame.

   If our current frame pointer is zero, we're at the top; else read out
   the saved FP from memory pointed to by the current FP.  */

#define FRAME_CHAIN(thisframe) ((thisframe)->frame? read_memory_integer ((thisframe)->frame, 4): 0)

/* If the chain pointer is zero (either because the saved value fetched
   by FRAME_CHAIN was zero, or because the current FP was zero so FRAME_CHAIN
   never fetched anything), we are at the top of the stack.  */

#define FRAME_CHAIN_VALID(chain, thisframe) (chain != 0)
