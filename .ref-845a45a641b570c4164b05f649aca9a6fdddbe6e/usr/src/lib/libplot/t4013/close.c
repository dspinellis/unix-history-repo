/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
closevt(){
	putch(037);
	fflush(stdout);
}
closepl(){
	putch(037);
	fflush(stdout);
}
