/* GNU Emacs site configuration file.
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


/* NOTE:  This is the config file for generic 4.3BSD.
	  If you want to make a change to the configuration file which
	  is included in the Emacs distribution, the file config.h.dist must
	  also be modified.
*/

/* Include here a s- file that describes the system type you are using.
   Alternatives now defined include
   s-bsd4.1.h, s-bsd4.2.h and s-bsd4.3.h for Berkeley systems,
   s-unipl5.2.h and s-unipl.5.0.h for Unisoft-modified system V,
   and s-usg5.2.h for ordinary losing system V.
   See s-template.h for documentation on writing s- files.  */
#include "s-bsd4.3.h"
 
/* Include here a m- file that describes the machine and system you use.
   Alternatives implemented so far are
    m-vax.h	for vaxen.
    m-mega68.h	for Megatest 68000's.
    m-sun.h	for Sun 68000's.
    m-ns16000.h	for 16000's (not sure how general this is yet)
    m-pyramid.h for pyramids.
    m-dual68.h  for dual 68000's (that run Unisoft port).
    m-apollo.h  for Apollo running their own system.
   See m-template.h for info on what m- files should define.
   */
#include "m-vax.h"


/* define HAVE_X_WINDOWS if you want to use the X window system */

#undef HAVE_X_WINDOWS

/* subprocesses should be defined if you want to
 have code for asynchronous subprocesses
 (as used in M-x compile and M-x shell).
 These do not work for some USG systems yet;
 for the ones where they work, the s-*.h file defines this flag.  */

#ifndef USG
#define subprocesses
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

#define AMPERSAND_FULL_NAME

/* Maximum screen width we handle. */

#define MScreenWidth 300

/* Maximum screen length we handle. */

#define MScreenLength 300

/* # bytes of pure Lisp code to leave space for.
  130000 is enough for Emacs as distributed by me, Richard Stallman,
  with the set of preloaded packages I set up.  */

#define PURESIZE 130000

/* Define HIGHPRI as a negative number
 if you want Emacs to run at a higher than normal priority.
 For this to take effect, you must install it as setuid root. */

#undef HIGHPRI
