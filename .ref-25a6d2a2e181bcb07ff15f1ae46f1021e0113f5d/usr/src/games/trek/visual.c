/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)visual.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  VISUAL SCAN
**
**	A visual scan is made in a particular direction of three sectors
**	in the general direction specified.  This takes time, and
**	Klingons can attack you, so it should be done only when sensors
**	are out.
*/

/* This struct[] has the delta x, delta y for particular directions */
struct xy	Visdelta[11] =
{
	-1,	-1,
	-1,	 0,
	-1,	 1,
	 0,	 1,
	 1,	 1,
	 1,	 0,
	 1,	-1,
	 0,	-1,
	-1,	-1,
	-1,	 0,
	-1,	 1
};

visual()
{
	register int		ix, iy;
	int			co;
	register struct xy	*v;

	co = getintpar("direction");
	if (co < 0 || co > 360)
		return;
	co = (co + 22) / 45;
	v = &Visdelta[co];
	ix = Ship.sectx + v->x;
	iy = Ship.secty + v->y;
	if (ix < 0 || ix >= NSECTS || iy < 0 || iy >= NSECTS)
		co = '?';
	else
		co = Sect[ix][iy];
	printf("%d,%d %c ", ix, iy, co);
	v++;
	ix = Ship.sectx + v->x;
	iy = Ship.secty + v->y;
	if (ix < 0 || ix >= NSECTS || iy < 0 || iy >= NSECTS)
		co = '?';
	else
		co = Sect[ix][iy];
	printf("%c ", co);
	v++;
	ix = Ship.sectx + v->x;
	iy = Ship.secty + v->y;
	if (ix < 0 || ix >= NSECTS || iy < 0 || iy >= NSECTS)
		co = '?';
	else
		co = Sect[ix][iy];
	printf("%c %d,%d\n", co, ix, iy);
	Move.time = 0.05;
	Move.free = 0;
}
