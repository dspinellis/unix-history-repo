/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)line.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
line(x0,y0,x1,y1){
	putc('l',stdout);
	putsi(x0);
	putsi(y0);
	putsi(x1);
	putsi(y1);
}
