/* m- file for Sun's 386-based RoadRunner.  This file borrows heavily from
  "m-sun2.h", but since that file is heavily cpu-specific, it was easier
  not to include it.

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

/* Say this machine is a bird */
#ifndef roadrunner
#define roadrunner
#endif

/* Actual cpu-specific defs */
#include "m-intel386.h"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Underscores are not prepended to C symbols on this machine.  */ 
#undef LDAV_SYMBOL
#define LDAV_SYMBOL "avenrun"

/* Must use the system's termcap.  It does special things.  */

#define LIBS_TERMCAP -ltermcap

/* Arrange to link with sun windows, if requested.  */
/* For details on emacstool and sunfns, see etc/SUN-SUPPORT */
/* These programs require Sun UNIX 4.2 Release 3.2 or greater */

#ifdef HAVE_SUN_WINDOWS
#define OTHER_FILES  ${etcdir}emacstool
#define LIBS_MACHINE -lsuntool -lsunwindow -lpixrect
#define OBJECTS_MACHINE sunfns.o
#define SYMS_MACHINE syms_of_sunfns ()
#define PURESIZE 132000
#endif

/* Roadrunner uses 'COFF' format */
#define COFF

#define C_SWITCH_MACHINE -Bstatic       /* avoid dynamic linking */
#define LD_SWITCH_MACHINE -n -Bstatic
/* Get rid of the -e __start that s-sunos4.h does.  */
#undef LD_SWITCH_SYSTEM
