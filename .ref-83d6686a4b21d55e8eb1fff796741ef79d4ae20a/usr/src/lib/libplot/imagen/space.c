/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)space.c	5.2 (Berkeley) %G%";
#endif not lint

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
