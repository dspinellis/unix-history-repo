/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
move(xi,yi){
	putc('m',stdout);
	putsi(xi);
	putsi(yi);
}
