/*	hpoint.c	1.4	83/07/09
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains routines for manipulating the point data
 * structures for the hard copy programs of the gremlin picture editor.
 */

#include "gprint.h"

/* imports from c */

extern char *malloc();

POINT *PTInit()
/*
 *      This routine creates a null point and returns  a pointer
 * to it.
 */

{
	register POINT *pt;

	pt = (POINT *) malloc(sizeof(POINT));
	pt->x = nullpt;
	pt->y = nullpt;
	return(pt);
}  /* end PTInit */

POINT *PTMakePoint(x, y, pointlist)
float x;
float y;
POINT *(*pointlist);
/*
 *      This routine creates a new point with coordinates x and y and 
 * links it into the pointlist.
 */

{
	register POINT *pt1;

	pt1 = *pointlist;
	while ( !Nullpoint(pt1) ) {
		pt1 = pt1->nextpt;
	}
	pt1->x = x;
	pt1->y = y;
	pt1->nextpt = PTInit();
        return(pt1);
}  /* end MakePoint */

