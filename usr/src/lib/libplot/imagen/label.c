/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	5.1 (Berkeley) 9/21/85";
#endif not lint

#include "imPcodes.h"
#include "imp.h"
extern imPcsize;
label(s)
char *s;
{
	register i,c;
	putch(imP_SET_ABS_H);
	putwd((int)((imPx-obotx)*scalex+botx)-imPcsize/2);
	putch(imP_SET_ABS_V);
	putwd((int)((imPy-oboty)*scaley+boty-(imPcsize/1.6)));
	for(i=0; c=s[i]; i++)
	{
		imPx += imPcsize/scalex;
		putch((c == ' ')?imP_SP:c);
	}
}
