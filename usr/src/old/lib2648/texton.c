/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)texton.c	5.1 (Berkeley) %G%";
#endif not lint

#include "2648.h"

texton()
{
	sync();
	escseq(TEXT);
}

textoff()
{
	sync();

	/*
	 * The following is needed because going into text mode
	 * leaves the pen where the cursor last was.
	 */
	_penx = -40; _peny = 40;
	escseq(ESCP);
	outchar('a');
	motion(_supx, _supy);
	_penx = _supx; _peny = _supy;
}
