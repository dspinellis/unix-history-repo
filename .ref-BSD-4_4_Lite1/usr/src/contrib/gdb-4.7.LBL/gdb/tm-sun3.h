/* Parameters for execution on a Sun, for GDB, the GNU debugger.
   Copyright (C) 1986, 1987, 1989, 1992 Free Software Foundation, Inc.

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

#define HAVE_68881

/* Let native-versus-cross support code know we are targeting sun3,
   and modify registers to include sun3 fpustate register.  */

#define GDB_TARGET_IS_SUN3 1

#include "tm-68k.h"

/* Offsets (in target ints) into jmp_buf.  Not defined by Sun, but at least
   documented in a comment in <machine/setjmp.h>! */

#define JB_ELEMENT_SIZE 4

#define JB_ONSSTACK 0
#define JB_SIGMASK 1
#define JB_SP 2
#define JB_PC 3
#define JB_PSL 4
#define JB_D2 5
#define JB_D3 6
#define JB_D4 7
#define JB_D5 8
#define JB_D6 9
#define JB_D7 10
#define JB_A2 11
#define JB_A3 12
#define JB_A4 13
#define JB_A5 14
#define JB_A6 15

/* Figure out where the longjmp will land.  Slurp the args out of the stack.
   We expect the first arg to be a pointer to the jmp_buf structure from which
   we extract the pc (JB_PC) that we will land at.  The pc is copied into ADDR.
   This routine returns true on success */

#define GET_LONGJMP_TARGET(ADDR) get_longjmp_target(ADDR)

#undef SAVED_PC_AFTER_CALL

#ifdef __STDC__
struct frame_info;
#endif

extern CORE_ADDR m68k_saved_pc_after_call PARAMS ((struct frame_info *));

#define SAVED_PC_AFTER_CALL(frame) \
  m68k_saved_pc_after_call(frame)
