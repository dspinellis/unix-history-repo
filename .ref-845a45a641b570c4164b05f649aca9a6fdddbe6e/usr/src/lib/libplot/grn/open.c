/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * converts plot to grn
 */

#include "grnplot.h"

int curx, cury;		/* Current world position */
int xbot=0, ybot=0;	/* Coordinates of screen lower-left corner */
double scale=1;		/* The number of pixels per 2**12 units
			 * of world coordinates.
			 */
int linestyle = 5;
int	ingrnfile = 0;
int	invector = 0;


/*---------------------------------------------------------
 *	Openpl initializes the graphics display and clears its screen.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */
openpl()
{}
