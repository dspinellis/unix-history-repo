/* Definitions file for GNU Emacs running on HPUX release 5.0.
   Based on AT&T System V.2.
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


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG				/* System III, System V, etc */

#define USG5

#define HPUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "hpux"

/* `nomultiplejobs' should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).

 On hpux this depends on the precise kind of machine in use,
 so the m- file defines this symbol if appropriate.  */

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptym/ptyp0  */

#define FIRST_PTY_LETTER 'p'

/*
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 */

#define HAVE_TERMIO

/*
 *	Define HAVE_TIMEVAL if the system supports the BSD style clock values.
 *	Look in <sys/time.h> for a timeval structure.
 */

#define HAVE_TIMEVAL

/* With HAVE_TIMEVAL define, Emacs expects to use `utimes'.
   But HPUX does not have one.  */

#define MISSING_UTIMES

/*
 *	Define HAVE_SELECT if the system supports the `select' system call.
 */

#define HAVE_SELECT

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

#define HAVE_SOCKETS

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/* Define this symbol if your system has the functions bcopy, etc.
 * s800 and later versions of s300 (s200) kernels have equivilents
 * of the BSTRING functions of BSD.  If your s200 kernel doesn't have
 * em comment out this section.
 */

#define BSTRING

/* subprocesses should be defined if you want to
 have code for asynchronous subprocesses
 (as used in M-x compile and M-x shell).
 This is generally OS dependent, and not supported
 under most USG systems.  */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Say we have the SYSV style of interprocess communication.  */

#define HAVE_SYSVIPC

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

/* Define SHORTNAMES if the C compiler can distinguish only
   short names.  It means that the stuff in ../shortnames
   must be run to convert the long names to short ones.

   Some USG systems support long names.
   If yours is one, DO NOT change this file!
   Do #undef SHORTNAMES in the m- file or in config.h.  */

/* #define SHORTNAMES */

/* We use the Berkeley (and usg5.2.2) interface to nlist.  */

#define NLIST_STRUCT

/* The file containing the kernel's symbol table is called /hp-ux.  */

#define KERNEL_FILE "/hp-ux"

/* The symbol in the kernel where the load average is found
   depends on the cpu type, so we let the m- files define LDAV_SYMBOL.  */

/* Special hacks needed to make Emacs run on this system.  */

/*
 *	Make the sigsetmask function go away.  Don't know what the
 *	ramifications of this are, but doesn't seem possible to
 *	emulate it properly anyway at this point.
 */

/* HPUX has sigsetmask */
/* #define sigsetmask(mask)	/ * Null expansion * / */

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

/* HP-UX has _setjmp and _longjmp */
/*
#define _setjmp setjmp
#define _longjmp longjmp
*/

/* On USG systems the system calls are interruptable by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define read sys_read
#define open sys_open
#define write sys_write

#define INTERRUPTABLE_OPEN
#define INTERRUPTABLE_IO

/* Use the system provided termcap(3) library */
#define TERMINFO

/* The 48-bit versions are more winning for Emacs.  */

#define rand lrand48
#define srand srand48

/* In hpux, the symbol SIGIO is defined, but the feature
   does not really exist.

   Here we assume that signal.h is included before config.h
   so that we can override it here.  */

#undef SIGIO

/* USG systems tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   Foil this.  Emacs carefully avoids static vars inside functions.  */

#define static

/* Define extra libraries to load.
   This should have -lBSD, but that library is said to make
   `signal' fail to work.  */

#ifdef HPUX_NET
#define LIBS_SYSTEM -ln
#else
#define LIBS_SYSTEM
#endif

/* Some additional system facilities exist.  */

#define HAVE_DUP2
#define HAVE_GETTIMEOFDAY
#define HAVE_VFORK
#define HAVE_RENAME

/* The following maps shared exec file to demand loaded exec.
   Don't do this as demand loaded exec is broken in hpux.  */

#if 0

/* Adjust a header field for the executable file about to be dumped.  */

#define ADJUST_EXEC_HEADER   \
  hdr.a_magic = ((ohdr.a_magic.file_type == OLDMAGIC.file_type) ?  \
		 NEWMAGIC : ohdr.a_magic);

#endif

/* Baud-rate values in tty status have nonstandard meanings.  */

#define BAUD_CONVERT  \
{ 0, 50, 75, 110, 135, 150, 200, 300, 600, 900, 1200,  \
  1800, 2400, 3600, 4800, 7200, 9600, 19200, 38400 }

/* This is needed for HPUX version 6.2; it may not be needed for 6.2.1.  */
#define SHORT_CAST_BUG

/* Define killpg so that we have process group functionality under HP.  */
#define	killpg(PGRP, KSIG)	kill (-(PGRP), KSIG)

#ifndef NOT_C_CODE
#ifndef NO_SHORTNAMES
#include <sys/wait.h>
#define WAITTYPE int
#endif
#define WRETCODE(w) (((w) >> 8) & 0377)
#endif
