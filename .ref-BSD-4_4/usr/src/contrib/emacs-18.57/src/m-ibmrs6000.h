/* R2 AIX machine/system dependent defines
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) ((((int) (c)) << 24) >> 24)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#define IBMR2AIX

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.	 */

#define NO_UNION_TYPE

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */
/* #define CANNOT_DUMP */

#define UNEXEC unexaix.o

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP

#define TEXT_START 0x10000000
#define TEXT_END 0
#define DATA_START 0x20000000
#define DATA_END 0

/* The data segment in this machine always starts at address 0x20000000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.	*/

#define DATA_SEG_BITS 0x20000000

#ifdef CANNOT_DUMP
/* Define shared memory segment symbols */

#define PURE_SEG_BITS 0x30000000

/* Use shared memory.  */
/* This is turned off because it does not always work.	See etc/AIX.DUMP.  */
/* #define HAVE_SHM */
#define SHMKEY 5305035		/* used for shared memory code segments */
#endif /* CANNOT_DUMP */

#define N_BADMAG(x) BADMAG(x)
#define N_TXTOFF(x) A_TEXTPOS(x)
#define N_SYMOFF(x) A_SYMPOS(x)
#define A_TEXT_OFFSET(HDR) sizeof(HDR)
/* #define ADJUST_EXEC_HEADER \
    unexec_text_start += sizeof(hdr); \
    unexec_data_start = ohdr.a_dbase
*/
#undef ADDR_CORRECT
#define ADDR_CORRECT(x) ((int)(x))

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.	 */

#define C_ALLOCA
#define STACK_DIRECTION -1 /* tell alloca.c which way it grows */

/* Specify the font for X to use.  */

#define X_DEFAULT_FONT "Rom14.500"

/* Here override various assumptions in ymakefile */

#define OBJECTS_MACHINE hftctl.o
#define C_SWITCH_MACHINE -D_BSD
#define LIBS_MACHINE -lrts
#define START_FILES
#define HAVE_DUP2
#define HAVE_GETTIMEOFDAY
#define HAVE_SYSVIPC
#define HAVE_SETSID
#define HAVE_GETWD
/*** BUILD 9008 - FIONREAD problem still exists in X-Windows. ***/
#define BROKEN_FIONREAD
#define NO_SIOCTL_H

#undef LINKER
#define LINKER cc -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp
