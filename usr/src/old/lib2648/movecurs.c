/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)movecurs.c	5.1 (Berkeley) 4/26/85";
#endif not lint

#include "2648.h"

movecurs(x, y)
{
	char mes[20];

	if (x==_curx && y==_cury)
		return;
	sprintf(mes, "%d,%do", x, y);
	escseq(ESCD);
	outstr(mes);
	escseq(NONE);
	_curx = x;
	_cury = y;
}
