/* Definitions file for GNU Emacs running on AT&T's System V.0
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
#define	USG5_0

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "usg-unix-v"

/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptyp0  */

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

/* #define HAVE_TIMEVAL */
 
/*
 *	Define HAVE_SELECT if the system supports the `select' system call.
 */

/* #define HAVE_SELECT */

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS */

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

/* #define HAVE_SOCKETS */

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

#define NONSYSTEM_DIR_LIBRARY

/* Define this symbol if your system has the functions bcopy, etc. */

/* #define BSTRING */

/* subprocesses should be defined if you want to
 have code for asynchronous subprocesses
 (as used in M-x compile and M-x shell).
 This is supposed to work now on system V release 2.  */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

/* Define SHORTNAMES if the C compiler can distinguish only
   short names.  It means that the stuff in ../shortnames
   must be run to convert the long names to short ones.  */

#define SHORTNAMES

/* We do NOT use the Berkeley (and usg5.2.2) interface to nlist.  */

/* #define NLIST_STRUCT */

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
   is named avenrun.  */

#define LDAV_SYMBOL "avenrun"

/* Special hacks needed to make Emacs run on this system.  */

/*
 *	Make the sigsetmask function go away.  Don't know what the
 *	ramifications of this are, but doesn't seem possible to
 *	emulate it properly anyway at this point.
 */

#define sigsetmask(mask)	/* Null expansion */

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

#define _setjmp setjmp
#define _longjmp longjmp

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

/* On USG systems these have different names */

#define index strchr
#define rindex strrchr

/* USG systems tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   Foil this.  Emacs carefully avoids static vars inside functions.  */

#define static

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (x)

/* Prevent -lg from being used for debugging.  Not implemented?  */

#define LIBS_DEBUG
