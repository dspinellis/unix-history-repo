/*
 *      VERS.C
 *      UTREE version and history.
 *      1.00    klin, Jul 12 1988 -- Initial version
 *      1.01    klin, Jul 17 1988 -- Remove filesystems completely
 *      1.02    klin, Feb  5 1990 -- Ported to BSD
 *                                   Filesystem independent directory handling
 *                                   Screen handling extracted from screen lib
 *      2.00    klin, Nov 10 1990 -- Changes and extensions
 *      2.01    klin, Jan 26 1991 -- Some bug fixes
 *      2.02    klin, Mar 23 1991 -- User defined commands
 *                                   File type commands
 *                                   Some minor changes and bug fixes
 *                    Apr  7 1991 -- File pattern extended
 *      3.00-um klin, Apr 20 1991 -- Bug fixes
 *                    May  5 1991 -- Some more extensions
 *      3.01-um klin, Jun  4 1991 -- File zooming added
 *                    Oct  7 1991 -- File size patterns added
 *                                   Bug in putchar() deleted
 *                    Oct 15 1991 -- Handling of symlinks changed
 *                                   Option -L added
 *                    Oct 26 1991 -- Marking directories changed
 *                                   Sorting and zooming filelists changed
 *                                   Print tree list added
 *                                   writedlist() changed
 *                                   Copying and moving files changed
 *                                   Select directory added
 *      3.02-um klin, Nov  1 1991 -- Screen layout changed
 *                                   Goto parent directory added
 *                                   Marking files changed
 *                                   Option -u changed to -n
 *                                   Option -u (update tree) added
 *              klin, Nov 10 1991 -- buildlist() changed
 *                                   Function key handling changed
 *              klin, Nov 24 1991 -- Some error fixes and extensions for XENIX
 *                                   reported and done by Rolf Gebhardt (Nov 22 1991)
 *                                   Video attributes changed
 *                                   Cd to current directory before executing
 *                                   some commands
 *      3.03-um klin, Feb 11 1992 -- Shell screen put into module hist.[ch]
 *                                   Shell command history added
 *                                   Status screen put into module stat.c
 *                                   Generic list type glist added
 *                                   Line editor extended, getline() and
 *                                   editline() changed
 *                                   Handling of variables and filetype
 *                                   commands changed
 *                                   Minor changes in screen layout
 *              klin, Feb 15 1992 -- Video handling changed, display attributes
 *                                   and video mode flag added
 *                                   Partitioning of directory and file window on
 *                                   tree screen changed and option -p added
 *              klin, Feb 22 1992 -- Many commands on tree screen changed
 *              klin, Feb 23 1992 -- Key handling and key bindings changed
 *                                   User customizable key bindings added
 *                                   Variables added
 *              klin, Mar  6 1992 -- Minor changes before distribution
 *            a klin, Mar 15 1992 -- Bug fixes and minor changes
 *            b klin, Mar 22 1992 -- Bug fixes and minor changes
 *            c klin, Mar 30 1992 -- More bug fixes
 *            d klin, Apr  2 1992 -- Time stamps for history added
 *            e klin, Apr 11 1992 -- Use colors for video attributes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03-um (klin) Feb 15 1992 vers.c";
#endif  /* !lint */
static char version[]   = "@(#) UTREE 3.03e-um";
static char copyright[] = "@(#) (C) 1991/92 P. Klingebiel & UNIX Magazin";
static char release[]   = "@(#) Release: Apr 11 1992";

#define VERS    &version[5]
#define COPY    &copyright[5]
#define DATE    &release[5]

#include "defs.h"

/* ---- Functions and procedures -------------------------------------- */

/* Write version string to echoline or helpline */
GLOBL int putversion(l, s)
  register int l;
  register char *s;
{
  if(l == helpline) {
    puthelp(" %s %s (%s)", VERS, COPY, DATE);
    return(RV_OK);
  }
  else if(s) {
    puthelp("%s %s", s, hitkey);
    (void) putecho("%s %s (%s)", VERS, COPY, DATE);
    return(hitakey(NULL));
  }

} /* putversion() */

/* Print version string */
GLOBL VOID utreeversion()
{
  (void) fprintf(stdout, "%s %s (%s)\n", VERS, COPY, DATE);

} /* utreeversion() */

/* Return utree version */
GLOBL char *getversion()
{
  return(VERS);

} /* getversion() */
