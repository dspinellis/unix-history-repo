/* Definitions for native support of irix4.

Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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
 * Let's use /debug instead of all this dangerous mucking about
 * with ptrace(), which seems *extremely* fragile, anyway.
 */
#define USE_PROC_FS
#define PROC_NAME_FMT "/debug/%d"

/* Don't need special routines for the SGI -- we can use infptrace.c */
#undef FETCH_INFERIOR_REGISTERS

#define U_REGS_OFFSET 0
