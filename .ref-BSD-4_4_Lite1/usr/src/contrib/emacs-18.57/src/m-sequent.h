/* m- file for SEQUENT BALANCE machines
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


/* NOTE: this file works for DYNIX release 2.0 
	  (not tested on 1.3) on NS32000's */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

/* #define BIG_ENDIAN */

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
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

/* BTW: DYNIX defines sequent, ns32000, and ns16000 (GENIX compatibility) */
#ifndef	sequent		/* pre DYNIX 2.1 releases */
# define sequent
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* crt0.c should use the vax-bsd style of entry, with these dummy args.  */

#define CRT0_DUMMIES bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE unsigned long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define	FSCALE	1000.0
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

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

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* #define C_ALLOCA */
#define HAVE_ALLOCA

/* Name of file the to look in
   for the kernel symbol table (for load average) */

#undef KERNEL_FILE
#define KERNEL_FILE "/dynix"

/* Avoids a compiler bug */

#define TAHOE_REGISTER_BUG

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  Furthermore, the value written
   in the a_text in the file must have N_ADDRADJ added to it.  */

#define A_TEXT_OFFSET(HDR) (sizeof (HDR) + N_ADDRADJ (HDR))

/* (short) negative-int doesn't sign-extend correctly */
#define SHORT_CAST_BUG

/* Cause compilations to be done in parallel in ymakefile.  */
#define MAKE_PARALLEL &

/* Say that mailer interlocking uses flock.  */
#define MAIL_USE_FLOCK

/* On many 4.2-based systems, there's a rather tricky bug
 * with the interpretation of the pid/pgrp value given to
 * the F_SETOWN fcntl() call.  It works as documented EXCEPT
 * when applied to filedescriptors for sockets, in which case
 * the sign must be reversed.  If your emacs subprocesses get
 * SIGIO's when they shouldn't, while running on a socket
 * (e.g. under X windows), you should probably define this.
 */

#define F_SETOWN_SOCK_NEG

/* Some really obscure 4.2-based systems (like Sequent DYNIX)
 * do not support asynchronous I/O (using SIGIO) on sockets,
 * even though it works fine on tty's.  If you have one of
 * these systems, define the following, and then use it in
 * config.h (or elsewhere) to decide when (not) to use SIGIO.
 */

#define NO_SOCK_SIGIO

/* Define how to search all pty names.
   This is for Dynix 3.0; delete next 5 definitions for older systems.  */

#define PTY_MAJOR "pqrstuvwPQRSTUVW"
#define PTY_MINOR "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define PTY_ITERATION					\
  register int ma, mi;					\
  for (ma = 0; ma < sizeof(PTY_MAJOR) - 1; ma++)	\
    for (mi = 0; mi < sizeof(PTY_MINOR) - 1; mi++)
#define PTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/pty%c%c", PTY_MAJOR[ma], PTY_MINOR[mi]);
#define PTY_TTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/tty%c%c", PTY_MAJOR[ma], PTY_MINOR[mi]);
