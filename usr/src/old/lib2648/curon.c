/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)curon.c	5.1 (Berkeley) %G%";
#endif not lint

#include "2648.h"

curon()
{
	if (_cursoron)
		return;
	sync();
	escseq(ESCD);
	outchar('k');
	_cursoron = 1;
}

curoff()
{
	if (!_cursoron)
		return;
	sync();
	escseq(ESCD);
	outchar('l');
	_cursoron = 0;
}
