/* Definitions to make GDB run on Mach on an Intel 386
   Copyright (C) 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

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

/* Avoid "INT_MIN redefined" warnings -- by defining it here, exactly
   the same as in the system <machine/machtypes.h> file.  */
#undef	INT_MIN
#define	INT_MIN		0x80000000

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

#define KERNEL_U_ADDR (0x80000000 - (UPAGES * NBPG))

#define BROKEN_LARGE_ALLOCA

#define PREPARE_TO_STORE() read_register_bytes (0, NULL, REGISTER_BYTES)

/* <errno.h> only defines this if __STDC__!!! */
extern int errno;

extern char *strdup();
