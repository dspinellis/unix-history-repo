/* Definitions file for GNU Emacs running on HPUX release 5.0.
   Based on AT&T System V.2.
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

/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptym/ptyp0  */

#define FIRST_PTY_LETTER 'p'

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
 *      HPUX has them, but csh and some other things work badly with them.
 */

#undef HAVE_PTYS

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/* Define this symbol if your system has the functions bcopy, etc. */

/* #define BSTRING */

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

/* Special hacks needed to make Emacs run on this system.  */

/* Define this to cause -N to be passed to ld.  This is needed
   in uniplus because of its funny memory space layout.  */

/* #define LOADER_N_SWITCH */

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

/* Use the system provided termcap(3) library */
#define TERMINFO

/* On USG systems these have different names */

#define index strchr
#define rindex strrchr

/* The 48-bit versions are more winning for Emacs.  */

#define rand lrand48
#define srand srand48

/* In hpux, for unknown reasons, S_IFLNK is defined
   even though symbolic links do not exist.
   Make sure our conditionals based on S_IFLNK are not confused.

   Here we assume that stat.h is included before config.h
   so that we can override it here.  */

#undef S_IFLNK

/* In hpux, the symbol SIGIO is defined, but the feature
   does not really exist.

   Here we assume that signal.h is included before config.h
   so that we can override it here.  */
  
#undef SIGIO

/* USG systems tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   Foil this.  Emacs carefully avoids static vars inside functions.  */

#define static
