/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cont.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include "imp.h"

cont(x,y){
	line(imPx, imPy, x, y);
	
}
