/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)point.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
point(xi,yi){
	putc('p',stdout);
	putsi(xi);
	putsi(yi);
}
