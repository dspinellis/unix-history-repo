/* Parameters for target machine Hewlett-Packard 9000/300, running bsd.
   Copyright (C) 1986, 1987, 1989, 1991 Free Software Foundation, Inc.

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

/*
 * Configuration file for HP9000/300 series machine running
 * University of Utah's 4.3bsd port.  This is NOT for HP-UX.
 * Problems to hpbsd-bugs@cs.utah.edu
 */

#define HAVE_68881

/* Define BPT_VECTOR if it is different than the default.
   This is the vector number used by traps to indicate a breakpoint. */

#define BPT_VECTOR 0x2

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

#define TARGET_NBPG 4096
#define TARGET_UPAGES 3

/* On the HP300, sigtramp is in the u area.  Gak!  User struct is not
   mapped to the same virtual address in user/kernel address space
   (hence STACK_END_ADDR as opposed to KERNEL_U_ADDR).  This tests
   for the whole u area, since we don't necessarily have hp300bsd
   include files around.  */
#define IN_SIGTRAMP(pc, name) \
  ((pc) >= STACK_END_ADDR   \
   && (pc) < STACK_END_ADDR + TARGET_UPAGES * TARGET_NBPG \
   )

/* Address of end of stack space.  */

#define STACK_END_ADDR 0xfff00000

#include "tm-68k.h"
