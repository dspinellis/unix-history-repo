/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)gon.c	5.1 (Berkeley) 4/30/85";
#endif not lint

#include "2648.h"

gon()
{
	sync();
	escseq(ESCD);
	outchar('c');
}

goff()
{
	sync();
	escseq(ESCD);
	outchar('d');
}
