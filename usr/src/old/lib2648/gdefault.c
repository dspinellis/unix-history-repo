/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)gdefault.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * reset terminal to default graphics state
 */

#include "2648.h"

gdefault()
{
	escseq(ESCM);
	outstr("r");
}
