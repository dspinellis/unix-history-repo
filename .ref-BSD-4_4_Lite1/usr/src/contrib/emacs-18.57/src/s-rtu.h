/* Definitions file for GNU Emacs running on RTU 3.0, ucb universe.
   Copyright (C) 1986 Free Software Foundation, Inc.

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

#define BSD4_2
#define BSD
#define RTU

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "rtu"

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.
   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   SIGIO can be used only on systems that implement it (4.2 and 4.3).
   CBREAK mode has two disadvatages
     1) At least in 4.2, it is impossible to handle the Meta key properly.
        I hear that in system V this problem does not exist.
     2) Control-G causes output to be discarded.
        I do not know whether this can be fixed in system V.

   Another method of doing input is planned but not implemented.
   It would have Emacs fork off a separate process
   to read the input and send it to the true Emacs process
   through a pipe.
*/

#undef INTERRUPT_INPUT

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'z'	/* i.e. no PTY_LETTERs */

/*
 *	Define HAVE_TIMEVAL if the system supports the BSD style clock values.
 *	Look in <sys/time.h> for a timeval structure.
 */

#define HAVE_TIMEVAL

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

#define NONSYSTEM_DIR_LIBRARY

/* Define this symbol if your system has the functions bcopy, etc. */
/* The system library bcopy() is broken in RTU.  For one thing, it expects
   the length to be less than 64k. */
#undef BSTRING

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#undef COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#undef MAIL_USE_FLOCK

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#undef CLASH_DETECTION

/* We use the Berkeley (and usg5.2.2) interface to nlist.  */

#define NLIST_STRUCT

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

/* Special hacks needed to make Emacs run on this system.  */

/* On RTU systems (like USG) the system calls are interruptable by signals
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

/* The "fsync" call on RTU versions 3.0 and 3.1 is badly broken!
   This hack below isn't the best solution, but without it this
   program will cause the whole system to hang!  !@#$#%$ Masscomp!  */

#define fsync(x) 0	/* "Comment out" fsync calls */

/* RTU has IPC instead of Unix-domain sockets.  */

#define HAVE_SYSVIPC
