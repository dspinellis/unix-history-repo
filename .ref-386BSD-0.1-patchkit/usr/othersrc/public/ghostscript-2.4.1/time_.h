/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* time_.h */
/* Generic substitute for Unix sys/time.h */

#include <sys/time.h>
#if defined(_IBMR2) || defined(SYSV) || defined(SVR4)		/* IBM RS/6000, AIX 3.n, System V.4 */
#  include <time.h>
#endif

#if defined(ultrix) && defined(mips)
/* Apparently some versions of Ultrix for the DECstation include */
/* time_t in sys/time.h, and some don't.  If you get errors */
/* compiling gp_unix.c, uncomment the next line. */
/*	typedef	int	time_t;	*/
#endif
