/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	5.1 (Berkeley) 9/21/85";
#endif not lint

#include "imPcodes.h"
#include "imp.h"
erase(){
	int i;
		putch(imP_ENDPAGE);
		imPx = imPy = 0;
		return;
}
