/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)video.c	5.1 (Berkeley) %G%";
#endif not lint

#include "2648.h"

vidnorm()
{
	_video = NORMAL;
}

vidinv()
{
	_video = INVERSE;
}

togvid()
{
	_video = (_video==NORMAL) ? INVERSE : NORMAL;
	escseq(ESCM);
	outstr("3a1b0 0 719 359e");
}
