/* Macro definitions for linux.
   Copyright (C) 1992 Free Software Foundation, Inc.
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

#if !defined (TM_LINUX_H)
#define TM_LINUX_H 1
/* number of traps that happen between exec'ing the shell
 * to run an inferior, and when we finally get to
 * the inferior code.  This is 2 on most implementations.
 */
#define START_INFERIOR_TRAPS_EXPECTED 2
#include "tm-i386v.h"
/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */
#define NAMES_HAVE_UNDERSCORE
#endif		/* !defined (TM_LINUX_H) */
