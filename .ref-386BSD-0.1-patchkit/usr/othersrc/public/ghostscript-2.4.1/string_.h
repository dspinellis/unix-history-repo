/* Copyright (C) 1989, 1990 Aladdin Enterprises.  All rights reserved.
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

/* string.h */
/* Generic substitute for Unix string.h */

#ifdef BSD4_2
#  include <strings.h>
#  define strchr index
#else
#  ifdef VMS
	extern char *strcat(), *strchr(), *strcpy(), *strupr();
	extern int strcmp(), strlen(), strncmp();
#  else
#    ifdef __MSDOS__
#      undef memset			/* see memory_.h */
#      include <string.h>
#      define memset(dest,chr,cnt) setmem(dest,cnt,chr)
#    else
#      include <string.h>
#    endif
#  endif
#endif
