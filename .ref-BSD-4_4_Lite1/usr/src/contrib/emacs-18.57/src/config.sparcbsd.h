/* GNU Emacs site configuration template file.
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



/* Include here a s- file that describes the system type you are using.
   See the file ../etc/MACHINES for a list of systems and
   the names of the s- files to use for them.
   See s-template.h for documentation on writing s- files.  */
#include "s-bsd4-4.h"

/* Include here a m- file that describes the machine and system you use.
   See the file ../etc/MACHINES for a list of machines and
   the names of the m- files to use for them.
   See m-template.h for info on what m- files should define.
   */
#include "m-sparcbsd.h"

/* Load in the conversion definitions if this system
   needs them and the source file being compiled has not
   said to inhibit this.  There should be no need for you
   to alter these lines.  */

#ifdef SHORTNAMES
#ifndef NO_SHORTNAMES
#include "../shortnames/remap.h"
#endif /* not NO_SHORTNAMES */
#endif /* SHORTNAMES */

/* Define HAVE_X_WINDOWS if you want to use the X window system.  */

#define HAVE_X_WINDOWS

/* Define X11 if you want to use version 11 of X windows.
   Otherwise, Emacs expects to use version 10.  */

#define X11

/* Define HAVE_X_MENU if you want to use the X window menu system.
   This appears to work on some machines that support X
   and not on others.  */

/* #define HAVE_X_MENU */

/* Define `subprocesses' should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   These do not work for some USG systems yet;
   for the ones where they work, the s-*.h file defines this flag.  */

#ifndef VMS
#ifndef USG
#define subprocesses
#endif
#endif

/* Define USER_FULL_NAME to return a string
   that is the user's full name.
   It can assume that the variable `pw'
   points to the password file entry for this user.

   At some sites, the pw_gecos field contains
   the user's full name.  If neither this nor any other
   field contains the right thing, use pw_name,
   giving the user's login name, since that is better than nothing.  */

#define USER_FULL_NAME pw->pw_gecos

/* Define AMPERSAND_FULL_NAME if you use the convention
   that & in the full name stands for the login id.  */

/* #define AMPERSAND_FULL_NAME */

/* # bytes of pure Lisp code to leave space for.
   Note that s-vms.h and m-sun2.h may override this default.  */

#ifndef PURESIZE
#ifdef HAVE_X_WINDOWS
#define PURESIZE 122000
#else
#define PURESIZE 119000
#endif
#endif

/* Define HIGHPRI as a negative number
   if you want Emacs to run at a higher than normal priority.
   For this to take effect, you must install Emacs with setuid root.
   Emacs will change back to the users's own uid after setting
   its priority.  */

/* #define HIGHPRI */

