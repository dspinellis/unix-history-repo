/* Macro definitions for GDB on an Intel i386 running SVR4.
   Copyright (C) 1991, Free Software Foundation, Inc.
   Written by Fred Fish at Cygnus Support (fnf@cygint)

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

/* Use the alternate method of determining valid frame chains. */

#define FRAME_CHAIN_VALID_ALTERNATE

/* number of traps that happen between exec'ing the shell 
 * to run an inferior, and when we finally get to 
 * the inferior code.  This is 2 on most implementations.
 */
#define START_INFERIOR_TRAPS_EXPECTED 2

/* Pick up most of what we need from the generic i386 target include file. */

#include "tm-i386v.h"

/* Pick up more stuff from the generic SVR4 host include file. */

#include "tm-sysv4.h"

/* We can't tell how many args there are
   now that the C compiler delays popping them.  */

#undef FRAME_NUM_ARGS
#define FRAME_NUM_ARGS(val,fi) (val = -1)

/* Offsets (in target ints) into jmp_buf.  Not defined in any system header
   file, so we have to step through setjmp/longjmp with a debugger and figure
   them out.  Note that <setjmp> defines _JBLEN as 10, which is the default
   if no specific machine is selected, even though we only use 6 slots. */

#define JB_ELEMENT_SIZE sizeof(int)	/* jmp_buf[_JBLEN] is array of ints */

#define JB_EBX	0
#define JB_ESI	1
#define JB_EDI	2
#define JB_EBP	3
#define JB_ESP	4
#define JB_EDX	5

#define JB_PC	JB_EDX	/* Setjmp()'s return PC saved in EDX */

/* Figure out where the longjmp will land.  Slurp the args out of the stack.
   We expect the first arg to be a pointer to the jmp_buf structure from which
   we extract the pc (JB_PC) that we will land at.  The pc is copied into ADDR.
   This routine returns true on success */

extern int
get_longjmp_target PARAMS ((CORE_ADDR *));

#define GET_LONGJMP_TARGET(ADDR) get_longjmp_target(ADDR)
