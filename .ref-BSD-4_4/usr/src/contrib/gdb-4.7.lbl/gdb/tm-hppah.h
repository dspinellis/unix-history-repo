/* Parameters for execution on an HP PA-RISC machine, running HPUX, for GDB.
   Copyright 1991, 1992 Free Software Foundation, Inc. 

   Contributed by the Center for Software Science at the
   University of Utah (pa-gdb-bugs@cs.utah.edu).

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

/* Mostly it's common to all HPPA's.  */
#include "tm-hppa.h"

/* Saved PC's are different, since there is millicode.  */
extern CORE_ADDR millicode_start, millicode_end;

/* Immediately after a function call, return the saved pc.
   Can't go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#undef	SAVED_PC_AFTER_CALL
#define SAVED_PC_AFTER_CALL(frame)              \
  ((get_frame_pc (frame) >= millicode_start     \
    && get_frame_pc (frame) < millicode_end) ?  \
   read_register (31) & ~3                      \
   : read_register (RP_REGNUM) & ~3)
