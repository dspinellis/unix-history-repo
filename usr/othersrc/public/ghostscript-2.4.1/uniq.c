/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
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

/* uniq.c */
/* A cheap MS-DOS substitute for just enough of the Unix uniq utility. */
#include <stdio.h>
#include <string.h>

/* Usage:
	uniq
 * Reads from stdin, writes to stdout, eliminating any line that is
 * equal to the previous line.
 */

main(int argc, char *argv[])
{	char x[200], y[200];
	char *a = x, *b = y, *t;
	b[0] = 0;
	while ( (*a = 0), gets(a), !feof(stdin) )
	   {	if ( strcmp(a, b) ) puts(a);
		t = a, a = b, b = t;
	   }
	if ( *a && strcmp(a, b) ) puts(a);
	return 0;
}
