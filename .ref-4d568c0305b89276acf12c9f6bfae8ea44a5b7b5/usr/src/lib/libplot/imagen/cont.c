/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cont.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include "imp.h"

cont(x,y){
	line(imPx, imPy, x, y);
	
}
