/* Macro defintions for i386, running System V 3.2.
   Copyright (C) 1989 Free Software Foundation, Inc.

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

#include "xm-i386v.h"

/* Apparently there is inconsistency among various System V's about what
   the name of this field is.  */
#define U_FPSTATE(u) u.u_fps.u_fpstate

/* TIOCGETC is defined in System V 3.2 termio.h, but struct tchars
   is not.  This makes problems for inflow.c.  */
#define TIOCGETC_BROKEN
