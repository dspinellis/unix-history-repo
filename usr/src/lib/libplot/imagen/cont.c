/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cont.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include "imp.h"

cont(x,y){
	line(imPx, imPy, x, y);
	
}
