/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)areaclear.c	5.1 (Berkeley) 4/26/85";
#endif not lint

#include "2648.h"

areaclear(rmin, cmin, rmax, cmax)
int rmin, cmin, rmax, cmax;
{
	int osm;
	char mes[20];
	register int i;

#ifdef TRACE
	if (trace)
		fprintf(trace, "areaclear(%d, %d, %d, %d)\n", rmin, cmin, rmax, cmax);
#endif
	osm = _supsmode;
	setclear();
	sync();
#ifdef notdef
	/* old kludge because I couldn't get area fill to work */
	for (i=rmax; i>=rmin; i--) {
		move(cmin, i);
		draw(cmax, i);
	}
#endif
	sprintf(mes, "%da1b%d %d %d %de", (_video==NORMAL) ? 1 : 2, cmin, rmin, cmax, rmax);
	escseq(ESCM);
	outstr(mes);
	_supsmode = osm;
}
