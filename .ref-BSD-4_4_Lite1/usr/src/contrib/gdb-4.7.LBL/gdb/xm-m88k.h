/* Host-machine dependent parameters for Motorola 88000, for GDB.
   Copyright 1986, 1987, 1988, 1989, 1990, 1991 Free Software Foundation, Inc.

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

#define HOST_BYTE_ORDER BIG_ENDIAN

#if !defined (USG)
#define USG 1
#endif

#include <sys/param.h>

#ifdef __GNUC__
#define memcpy __builtin_memcpy
/* gcc doesn't have this, at least not gcc 1.92.  */
/* #define memset __builtin_memset */
#define strcmp __builtin_strcmp
#endif

#ifdef DGUX
#define x_foff _x_x._x_offset
#define x_fname _x_name
#define USER ptrace_user
#define _BSD_WAIT_FLAVOR
#endif

#define HAVE_TERMIO

#ifndef USIZE
#define USIZE 2048
#endif
#define NBPG NBPC
#define UPAGES USIZE

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

/* Since registers r0 through r31 are stored directly in the struct ptrace_user,
   (for m88k BCS)
   the ptrace_user offsets are sufficient and KERNEL_U_ADDRESS can be 0 */

#define KERNEL_U_ADDR 0

