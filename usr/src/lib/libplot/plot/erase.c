/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
erase(){
	putc('e',stdout);
}
