/* m- file for hp9000 series 800 machines.
   Copyright (C) 1987 Free Software Foundation, Inc.

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

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#undef WORD_MACHINE

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef hp9000s800
#	define hp9000s800
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) (x * 100.0))


/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

#define VIRT_ADDR_VARIES

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
/* #define HAVE_ALLOCA */

/* the data segment on this machine always starts at address 0x40000000. */

#define DATA_SEG_BITS 0x40000000

#define VALBITS    26
#define GCTYPEBITS 5

#define DATA_START    0x40000000
#define TEXT_START    0x00000000

#define STACK_DIRECTION 1 

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#define UNEXEC unexhp9k800.o

#define LIBS_MACHINE
#define LIBS_DEBUG

/* Define NEED_BSDTTY if you have such. */

#define NEED_BSDTTY

/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short. */
   

#define XUINT(a) (((unsigned)(a) << INTBITS-VALBITS) >> INTBITS-VALBITS)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + (((unsigned) (ptr) << INTBITS-VALBITS) >> INTBITS-VALBITS))

#define XSETINT(a, b)  XSET(a, XTYPE(a), b)
#define XSETUINT(a, b) XSET(a, XTYPE(a), b)
#define XSETPNTR(a, b) XSET(a, XTYPE(a), b)
#define XMARKBIT(a) ((a) < 0)
#define XSETMARKBIT(a,b) ((a) = ((b) ? (a)|MARKBIT : (a) & ~MARKBIT))

#if 0  /* Loses when sign bit of type field is set.  */
#define XUNMARK(a) ((a) = (((a) << INTBITS-GCTYPEBITS-VALBITS) >> INTBITS-GCTYPEBITS-VALBITS))
#endif

/* The symbol in the kernel where the load average is found
   is named _avenrun.  At this time there are two major flavors
   of hp-ux (there is the s800 and s300 (s200) flavors).  The
   differences are thusly moved to the corresponding m- file.
*/

/* no underscore please */
#define LDAV_SYMBOL "avenrun"
#define CPTIME_SYMBOL "cp_time"
#define DKXFER_SYMBOL "dk_xfer"

/* In hpux, for unknown reasons, S_IFLNK is defined even though
   symbolic links do not exist.
   Make sure our conditionals based on S_IFLNK are not confused.

   Here we assume that stat.h is included before config.h
   so that we can override it here.  */

#undef S_IFLNK

/* Define the BSTRING functions in terms of the sysV functions. */

#define bcopy(a,b,s)	memcpy (b,a,s)
#define bzero(a,s)	memset (a,0,s)
#define bcmp		memcmp

/* On USG systems these have different names. */

#define index strchr
#define rindex strrchr

/* Include the file bsdtty.h, since job control exists.  */
#define NEED_BSDTTY
