/* Parameters for hosting on an HPPA-RISC machine running HPUX, for GDB.
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

/* Host is big-endian. */
#define	HOST_BYTE_ORDER	BIG_ENDIAN

/* Avoid "INT_MIN redefined" warnings -- by defining it here, exactly
   the same as in the system <machine/machtypes.h> file.  */
#undef  INT_MIN
#define INT_MIN         0x80000000

#ifndef hp800
#define USG
#endif

#ifndef __STDC__
/* This define is discussed in decode_line_1 in symtab.c  */
#define HPPA_COMPILER_BUG
#endif

#define HAVE_TERMIO

#define KERNEL_U_ADDR 0

/* HP uses non-ANSI definitions, but with void * results.  */
#define	MEM_FNS_DECLARED	/* Some non-ANSI use void *, not char *.  */
extern void *
memcpy PARAMS ((void *, const void *, size_t));		/* 4.11.2.1 */

extern void *
memset PARAMS ((void *, int, size_t));			/* 4.11.6.1 */

