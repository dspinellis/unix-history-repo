/* Macro definitions for i386 running under BSD Unix.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

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

/* Override number of expected traps from sysv. */
#define START_INFERIOR_TRAPS_EXPECTED 2

/* Most definitions from sysv could be used. */
#include "tm-i386v.h"

#ifdef notdef
/* 386BSD cannot handle the segment registers. */
#undef NUM_REGS
#define NUM_REGS 12
#endif

/*	BSDI $Id: tm-i386bsd.h,v 1.1.1.1 1992/08/27 17:03:50 trent Exp $	*/

#undef	FRAME_NUM_ARGS
#define	FRAME_NUM_ARGS(val, fi)		(val = -1)

#undef	COFF_NO_LONG_FILE_NAMES
#define	NAMES_HAVE_UNDERSCORE		1
#define	NEED_TEXT_START_END		1

#ifdef KERNELDEBUG
#define	MEM_DEVICE			2

extern int kernel_debugging;
#undef FRAME_CHAIN_VALID
#define FRAME_CHAIN_VALID(chain, thisframe) \
	(chain != 0 && \
	 kernel_debugging ? inside_kernstack(chain) : \
		(!inside_entry_file(FRAME_SAVED_PC(thisframe))))
#endif
