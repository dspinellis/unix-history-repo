/* Examine the result of  stat  and make a string describing file modes.
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


#include <sys/types.h>
#include <sys/stat.h>

/* filemodestring - set file attribute data 

*** WARNING!  FILE STRUCTURE DEPENDENT ***

   Filemodestring converts the data in the st_mode field of file status
block `s' to a 10 character attribute string, which it stores in
the block that `a' points to.
This attribute string is modelled after the string produced by the Berkeley ls.

As usual under Unix, the elements of the string are numbered
from 0.  Their meanings are:

   0	File type.  'd' for directory, 'c' for character
	special, 'b' for block special, 'm' for multiplex, '-'
	for any other file type

   1	'r' if the owner may read, '-' otherwise.

   2	'w' if the owner may write, '-' otherwise.

   3	'x' if the owner may execute, 's' if the file is
	set-user-id, '-' otherwise.

   4	'r' if group members may read, '-' otherwise.

   5	'w' if group members may write, '-' otherwise.

   6	'x' if group members may execute, 's' if the file is
	set-group-id, '-' otherwise.

   7	'r' if any user may read, '-' otherwise.

   8	'w' if any user may write, '-' otherwise.

   9	'x' if any user may execute, 't' if the file is "sticky"
	(will be retained in swap space after execution), '-'
	otherwise.

 */

#define TEXT char
#define VOID void

static TEXT ftypelet ();
static VOID rwx (), setst ();

VOID
filemodestring (s,a)
   struct stat	*s;
   TEXT *a;
{
   a[0] = ftypelet(s);
   rwx ((s->st_mode&0700)<<0, &(a[1]));
   rwx ((s->st_mode&0070)<<3, &(a[4]));
   rwx ((s->st_mode&0007)<<6, &(a[7]));
   setst (s->st_mode, a);
}

/* ftypelet - file type letter

*** WARNING!  FILE STRUCTURE DEPENDENT ***

   Ftypelet accepts a file status block and returns a character
code describing the type of the file.  'd' is returned for
directories, 'b' for block special files, 'c' for character
special files, 'm' for multiplexor files, and '-' for regular
files.

 */

static TEXT
ftypelet(s)
   struct stat *s;
{
   
   if ((s->st_mode&S_IFMT)==S_IFDIR)
      return 'd';
   else if ((s->st_mode&S_IFMT)==S_IFCHR)
      return 'c';
   else if ((s->st_mode&S_IFMT)==S_IFBLK)
      return 'b';
/* These do not seem to exist */
/*   else if ((s->st_mode&S_IFMT)==S_IFMPC ||
	    (s->st_mode&S_IFMT)==S_IFMPB)
      return 'm';
 */
   else
      return '-';
}


/* rwx - look at read, write, and execute bits and set character
flags accordingly

*** WARNING!  FILE STRUCTURE DEPENDENT ***

 */

static VOID
rwx (bits, chars)
   unsigned short bits;
   TEXT chars[];
{
    chars[0] = chars[1] = chars[2] = '-';
   if (bits&S_IREAD)
      chars[0] = 'r';
   if (bits&S_IWRITE)
      chars[1] = 'w';
   if (bits&S_IEXEC)
      chars[2] = 'x';
}


/* setst - set s & t flags in a file attributes string */
/* *** WARNING!  FILE STRUCTURE DEPENDENT *** */
static VOID
setst (bits, chars)
   unsigned short bits;
   TEXT chars[];
{
   if (bits&S_ISUID)
      chars[3] = 's';
   if (bits&S_ISGID)
      chars[6] = 's';
   if (bits&S_ISVTX)
      chars[9] = 't';
}
