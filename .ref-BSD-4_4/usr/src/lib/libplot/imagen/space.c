/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)space.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

extern float botx;
extern float boty;
extern float obotx;
extern float oboty;
extern float scalex;
extern float scaley;

int PlotRes = DEFRES;

int scaleflag;
space(x0,y0,x1,y1){
	botx = 2.;
	boty = 2.;
	obotx = x0;
	oboty = y0;
	if(scaleflag)
		return;
	scalex = (8.0 * PlotRes)/(x1-x0);
	scaley = (8.0 * PlotRes)/(y1-y0);
}
