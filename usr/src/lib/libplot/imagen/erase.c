/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "imPcodes.h"
#include "imp.h"
erase(){
	int i;
		putch(imP_ENDPAGE);
		imPx = imPy = 0;
		return;
}
