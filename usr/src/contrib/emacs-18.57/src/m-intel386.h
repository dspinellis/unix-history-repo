/* m- file for intel 386.
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

/* i386 is not big-endian: lowest numbered byte is least significant. */

/* #undef BIG_ENDIAN */

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
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#define INTEL386

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG and XENIX,
   NO_REMAP says this isn't used. */

#define CRT0_DUMMIES bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

/* USG systems do not actually support the load average,
so disable it for them.  */

#ifdef XENIX
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE short

/* Convert that into an integer that is 100 for a load average of 1.0  */
#define LOAD_AVE_CVT(x) (((double) (x)) * 100.0 / FSCALE)
  
#define FSCALE 256.0         /* determined by experimentation...  */
#endif

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

#ifdef XENIX
#define VALBITS 26
#define GCTYPEBITS 5

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

#define STACK_DIRECTION -1

/* Since cannot purify, use standard Xenix 386 startup code. */

#define START_FILES	/lib/386/Sseg.o pre-crt0.o /lib/386/Scrt0.o

/* These really use terminfo.  */

#define LIBS_TERMCAP /lib/386/Slibcurses.a  \
   /lib/386/Slibtinfo.a /lib/386/Slibx.a

/* Standard libraries for this machine.  Since `-l' doesn't work in `ld'.  */
/* '__fltused' is unresolved w/o Slibcfp.a */
#define LIB_STANDARD /lib/386/Slibcfp.a /lib/386/Slibc.a
#else /* not XENIX */

#ifdef USG
#ifndef LIB_STANDARD
#define LIB_STANDARD -lPW -lc
#endif
#define HAVE_ALLOCA
#define NO_REMAP 
#define TEXT_START 0
#endif /* USG */
#endif /* not XENIX */

#ifdef BSD
#define HAVE_ALLOCA
#endif /* BSD */

/* If compiling with GCC, let GCC implement alloca.  */
#if defined(__GNUC__) && !defined(alloca)
#define alloca(n) __builtin_alloca(n)
#define HAVE_ALLOCA
#endif
