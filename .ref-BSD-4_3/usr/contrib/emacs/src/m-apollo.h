/* m- file for Apollo machine.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* 68000 has lowest-numbered byte as most significant */

#define BIG_ENDIAN

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Say this machine is a 68000 */

/* #define m68000 */   /* Done by the C compiler */

#define APOLLO

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no /dev/kmem */

/* Define CANNOT_DUMP because it is impossible to do unexec.  */

#define CANNOT_DUMP

/* Define VIRT_ADDR_VARIES because the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.  */

#define VIRT_ADDR_VARIES

/* Define HAVE_ALLOCA because we use the system's version of alloca.  */

#define HAVE_ALLOCA

/* DN460 has a 28 bit virtual address space, and 26 bits are often used */

#define VALBITS 26
#define GCTYPEBITS 5
