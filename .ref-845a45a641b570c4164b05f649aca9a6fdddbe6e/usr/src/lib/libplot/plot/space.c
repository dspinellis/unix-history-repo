/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)space.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
space(x0,y0,x1,y1){
	putc('s',stdout);
	putsi(x0);
	putsi(y0);
	putsi(x1);
	putsi(y1);
}
