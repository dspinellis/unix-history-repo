/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)dumb.h	8.1 (Berkeley) %G%
 */

/*
 * This accepts plot file formats and produces the appropriate plots
 * for dumb terminals.  It can also be used for printing terminals and
 * lineprinter listings, although there is no way to specify number of
 * lines and columns different from your terminal.  This would be easy
 * to change, and is left as an exercise for the reader.
 */

#include <math.h>

#define scale(x,y) y = LINES-1-(LINES*y/rangeY +minY); x = COLS*x/rangeX + minX

extern int minX, rangeX;	/* min and range of x */
extern int minY, rangeY;	/* min and range of y */
extern int currentx, currenty;
extern int COLS, LINES;

/* A very large screen! (probably should use malloc) */
#define MAXCOLS		132
#define MAXLINES	90

extern char screenmat[MAXCOLS][MAXLINES];
