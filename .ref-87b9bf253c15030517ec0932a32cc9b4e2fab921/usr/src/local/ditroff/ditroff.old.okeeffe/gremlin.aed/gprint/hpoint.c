
/* hpoint.c -
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains routines for manipulating the point data
 * structures for the hard copy programs of the gremlin picture editor.
 */

#include "gprint.h"
#include "grem2.h"

/* imports from c */

extern char *malloc();

POINT *PTInit()
/*
 *      This routine creates a null point and returns  a pointer
 * to it.
 */

{
	POINT *pt;

	pt = (POINT *) malloc(sizeof(POINT));
	pt->x = nullpt;
	pt->y = nullpt;
	return(pt);
}  /* end PTInit */

POINT *PTMakePoint(x, y, pointlist)
float x, y;
POINT *(*pointlist);
/*
 *      This routine creates a new point with coordinates x and y and 
 * links it into the pointlist.
 */

{
	POINT *pt1;

	pt1 = *pointlist;
	while ( !Nullpoint(pt1) )
	{
		pt1 = pt1->nextpt;
	}  /* end while */;
	pt1->x = x;
	pt1->y = y;
	pt1->nextpt = PTInit();
        return(pt1);
}  /* end MakePoint */

