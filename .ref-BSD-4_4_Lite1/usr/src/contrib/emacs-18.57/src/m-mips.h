/* m- file for Mips machines.
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

#define SIGN_EXTEND_CHAR(c) ((signed char)(c))

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef mips
#	define mips
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

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / 256.0)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
/* #define HAVE_ALLOCA */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#define UNEXEC unexmips.o

/* Describe layout of the address space in an executing process.  */

#define TEXT_START 0x400000
#define DATA_START 0x800000

/* Alter some of the options used when linking.  */

#ifdef BSD

/* Supposedly the dec machine doesn't have this library.
   #define LIBS_MACHINE -lmld  */

#define LD_SWITCH_MACHINE -D 800000
#define LIBS_DEBUG

#else

#define LIBS_MACHINE -lmld
#define LD_SWITCH_MACHINE -D 800000 -g3
#define START_FILES pre-crt0.o /usr/lib/crt1.o
#define LIB_STANDARD -lbsd -lc /usr/lib/crtn.o
#define LIBS_TERMCAP -lcurses

#define C_SWITCH_SYSTEM -I/usr/include/bsd
#define C_DEBUG_SWITCH -O -g3

#if defined(HAVE_X_WINDOWS) && defined(HAVE_X11)
#define HAVE_VFORK		/* Graciously provided by libX.a */
#endif

#endif

/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short.  */

#define XUINT(a) (((unsigned)(a) << INTBITS-VALBITS) >> INTBITS-VALBITS)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + (((unsigned) (ptr) << INTBITS-VALBITS) >> INTBITS-VALBITS))

#define XSETINT(a, b)  XSET(a, XTYPE(a), b)
#define XSETUINT(a, b) XSET(a, XTYPE(a), b)
#define XSETPNTR(a, b) XSET(a, XTYPE(a), b)

#define XUNMARK(a) ((a) = (((unsigned)(a) << INTBITS-GCTYPEBITS-VALBITS) >> INTBITS-GCTYPEBITS-VALBITS))

#ifdef USG

/* Cancel certain parts of standard sysV support.  */
#undef NONSYSTEM_DIR_LIBRARY
#define SYSV_SYSTEM_DIR
#undef static

/* Don't try to use SIGIO or FIONREAD even though they are defined.  */
#undef SIGIO
#define BROKEN_FIONREAD

/* Describe special kernel features.  */

#define HAVE_SYSVIPC

#define HAVE_TIMEVAL
#if defined(emacs)
#include <bsd/sys/time.h>
#endif

/* #define HAVE_SELECT
   The `select' in the system won't work for pipes,
   so don't use it.  */

#define HAVE_DUP2
#define HAVE_GETWD
#define HAVE_GETTIMEOFDAY

#define HAVE_PTYS
#define HAVE_SOCKETS

#undef NOMULTIPLEJOBS
#define utimes utime  /* Someone should check this.  */

/* ??? */
#define IRIS

#endif

#ifdef BSD
#define COFF
#define TERMINFO
#undef MAIL_USE_FLOCK  /* Someone should check this.  */
#undef HAVE_UNION_WAIT
#endif /* BSD */

/* Load average requires special crocks.  Version 19 has them.
   For now, this avoids a bug.  */
#undef LOAD_AVE_TYPE
