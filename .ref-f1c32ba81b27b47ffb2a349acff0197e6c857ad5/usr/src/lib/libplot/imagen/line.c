/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)line.c	5.2 (Berkeley) %G%";
#endif not lint

#include "imp.h"
#include "imPcodes.h"
float obotx = 0.;
float oboty = 0.;
float botx = 2.;
float boty = 2.;
float scalex = 1.;
float scaley = 1.;
line(x0,y0,x1,y1)
{
	putch(imP_CREATE_PATH);
	putwd(2);		/* two coordinates follow */
	putwd((int)((x0-obotx)*scalex+botx));	
	putwd((int)((y0-oboty)*scaley+boty));	
	putwd((int)((x1-obotx)*scalex+botx));	
	putwd((int)((y1-oboty)*scaley+boty));	
	putch(imP_DRAW_PATH);
	putch(15);		/* "black" lines */
	imPx = x1;
	imPy = y1;
}
putch(c)
{
	putc(c, stdout);
}
putwd(w)
{
	putch(w>>8);
	putch(w);
}
