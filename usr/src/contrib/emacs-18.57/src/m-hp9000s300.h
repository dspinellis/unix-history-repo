/* m- file for hp9000 series 200 or 300
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



/* Define NOMULTIPLEJOBS on versions of HPUX before 6.5.  */

/* #define NOMULTIPLEJOBS */

/* Define this symbol if you are running a version of HP-UX
   which predates version 6.01 */

/* #define HPUX_5 */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#ifndef BIG_ENDIAN
#define BIG_ENDIAN
#endif

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/* #define NO_ARG_ARRAY */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef hp9000s300
#define hp9000s300
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

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* For University of Utah 4.3bsd and standard 4.4bsd on HP300s.  */

#ifdef BSD4_3
/* Tell crt0.c that this is an ordinary 68020.  */
#undef hp9000s300
#define CRT0_DUMMIES		bogus_a6,

#define HAVE_ALLOCA

#ifndef HAVE_GETLOADAVG
#undef LOAD_AVE_TYPE
#undef LOAD_AVE_CVT
#define LOAD_AVE_TYPE long
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) / 2048.0 * 100.0))
#endif

#endif /* BSD4_3 */

#ifndef BSD4_3
/* The following definitions are for HPUX only.  */

/* The symbol in the kernel where the load average is found
   is named _avenrun on this machine.  */

#define LDAV_SYMBOL "_avenrun"

/* This library is needed with -g, on the 200/300 only.  */

#if !defined(__GNUC__) || defined(__HPUX_ASM__)
#define LIBS_DEBUG /usr/lib/end.o
#endif

/* The symbol FIONREAD is defined, but the feature does not work
   on the 200/300.  */

#define BROKEN_FIONREAD

/* In older versions of hpux, for unknown reasons, S_IFLNK is defined
   even though symbolic links do not exist.
   Make sure our conditionals based on S_IFLNK are not confused.

   Here we assume that stat.h is included before config.h
   so that we can override it here.

   Version 6 of HP-UX has symbolic links.  */

#ifdef HPUX_5
#undef S_IFLNK
#endif

/* Define the BSTRING functions in terms of the sysV functions.
   Version 6 of HP-UX supplies these in the BSD library,
   but that library has reported bugs in `signal'.  */

/* #ifdef HPUX_5 */
#define bcopy(a,b,s)	memcpy (b,a,s)
#define bzero(a,s)	memset (a,0,s)
#define bcmp		memcmp
/* #endif */

/* On USG systems these have different names.
   Version 6 of HP-UX supplies these in the BSD library,
   which we currently want to avoid using.  */

/* #ifdef HPUX_5 */
#define index strchr
#define rindex strrchr
/* #endif */

/* Define C_SWITCH_MACHINE to be +X if you want the s200/300
 * Emacs to run on both 68010 and 68020 based hp-ux's.
 *
 * Define OLD_HP_ASSEMBLER if you have an ancient assembler
 *
 * Define HPUX_68010 if you are using the new assembler but
 * compiling for a s200 (upgraded) or s310.  68010 based
 * processor without 68881.
 */

/* These switches increase the size of some internal C compiler tables.
   They are required for compiling the X11 interface files. */

#ifndef HPUX_5
#ifndef __GNUC__
#define C_SWITCH_MACHINE -Wc,-Nd4000,-Ns3000
#endif
#endif

/* Define NEED_BSDTTY if you have such. */

#ifndef NOMULTIPLEJOBS
#define NEED_BSDTTY
#endif

#endif /* not BSD4_3 */
