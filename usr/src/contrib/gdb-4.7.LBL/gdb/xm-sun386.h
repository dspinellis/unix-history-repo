/* Host support for Sun 386i, for GDB, the GNU debugger.
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

#define HOST_BYTE_ORDER LITTLE_ENDIAN

/* Get rid of any system-imposed stack limit if possible.  */

/* If I do this on SunOS 4.0.1, I get SIGSEGV's on (some) instructions which
   try to access the stack.  */
/* #define SET_STACK_LIMIT_HUGE */

#define BROKEN_LARGE_ALLOCA

/* Enable use of alternate code for Sun's format of core dump file.  */

#define NEW_SUN_CORE

#define PREPARE_TO_STORE() read_register_bytes (0, NULL, REGISTER_BYTES)
