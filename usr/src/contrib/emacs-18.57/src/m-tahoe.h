/* m- file for tahoe.
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* lowest-numbered byte is most significant */

#define BIG_ENDIAN

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Say this machine is a tahoe */

#ifndef tahoe
#define tahoe
#endif /* not tahoe */

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* crt0.c should use the vax-bsd style of entry, with no dummy args.  */

#define CRT0_DUMMIES

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* This triggers some stuff to avoid a compiler bug */

#define TAHOE_REGISTER_BUG

/* System provides alloca.  */

#define HAVE_ALLOCA

/* Control header files used by loadst.c.
   Some users report machines have dkstat.h while others report dk.h,
   so it's hard to tell what this should say.  */

#ifdef BSD
#define DKSTAT_HEADER_FILE
#endif
