/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)zoomon.c	5.1 (Berkeley) %G%";
#endif not lint

#include "2648.h"

zoomon()
{
	escseq(ESCD);
	outchar('g');
}

zoomoff()
{
	escseq(ESCD);
	outchar('h');
}
