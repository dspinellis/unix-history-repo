/*******************************************************************
*                                                                  *
*    File: CIFPLOT/clip.c                                          *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"
#include "out_structs.h"
#include "alloc.h"

#define YINTERCEPT(y)	(x2 + ((x1 - x2)*((y) - y2))/(y1 - y2))

Clip(e)
iedge *e;
/* Clip edge and send to queue */
{
    register int x1,y1,x2,y2;	/* edges go from (x1,y1) to (x2,y2) */
    int x;			/* x is a temporary */

    x1 = e->x1; y1 = e->y1;
    x2 = e->x2; y2 = e->y2;
    /* If totally out of range return */
    if((x1 >= Top) || (x2 <= Bottom)) {
	return;
	}
    /* Clip against xmin */
    if(x1 < Bottom) {
	y1 = y2 + ((y1 - y2)*(Bottom - x2))/(x1 - x2);
	x1 = Bottom;
	}
    
    /* Clip against xmax */
    if(x2 > Top) {
	y2 = y1 + ((y2 - y1)*(Top - x1))/(x2 - x1);
	x2 = Top;
	}

    /* Determine slope of line and branch */
    if(y1 > y2) {
	/* Check that an end point is in a valid area */
	if(y1 < 0) y1 = y2 = 0;
	if(y2 > NoPixcels) y1 = y2 = NoPixcels;

	/* Clip against ymax */
	if(y1 > NoPixcels) {
		x = YINTERCEPT(NoPixcels);
		if(x1 != x2) {
		    if(x1 <= e->x1)
		        SendEdge(x1,NoPixcels,x,NoPixcels,e->dir,e->poly);
		      else
		        DupEdge(x1,NoPixcels,x,NoPixcels,e->dir,e->poly);
		    }
		x1 = x; y1 = NoPixcels;
		}
	/* Clip against ymin */
	if(y2 < 0) {
		x = YINTERCEPT(0);
		if(x1 != x2) {
		    if(x <= e->x1)
		        SendEdge(x,0,x2,0,e->dir,e->poly);
		      else
		        DupEdge(x,0,x2,0,e->dir,e->poly);
		    }
		x2 = x; y2 = 0;
		}
	}
    if(y1 < y2) {
	/* Check that an end point is in a valid area */
	if(y2 < 0) y1 = y2 = 0;
	if(y1 > NoPixcels) y1 = y2 = NoPixcels;

	/* Clip against ymin */
	if(y1 < 0) {
		x = YINTERCEPT(0);
		if(x1 != x2) {
		    if(x1 <= e->x1)
		        SendEdge(x1,0,x,0,e->dir,e->poly);
		      else
		        DupEdge(x1,0,x,0,e->dir,e->poly);
		    }
		x1 = x; y1 = 0;
		}
	/* Clip against ymax */
	if(y2 > NoPixcels) {
		x = YINTERCEPT(NoPixcels);
		if(x1 != x2) {
		    if(x <= e->x1)
		        SendEdge(x,NoPixcels,x2,NoPixcels,e->dir,e->poly);
		      else
		        DupEdge(x,NoPixcels,x2,NoPixcels,e->dir,e->poly);
		    }
		x2 = x; y2 = NoPixcels;
		}
	}
    if(y1 == y2) {
	/* y1 == y2 is treated seperately since it would cause
	 * divide by zero in YINTERCEPT */
	if(y1 < 0) y1 = y2 = 0;
	if(y2 > NoPixcels) y1 = y2 = NoPixcels;
	}

    /* Send clipped edges to queue */
    if(x1 <= e->x1)
        SendEdge(x1,y1,x2,y2,e->dir,e->poly);
      else
	DupEdge(x1,y1,x2,y2,e->dir,e->poly);
    return;
    }

int LastA = -999999;
SendEdge(a,b,c,d,dir,poly)
int a,b,c,d,dir;
PolyDesc *poly;
/* put the edge (a,b) -- (c,d) into queue */
{
    edge *e;

    /* Clip */
    if(a == c && poly->level != 0) return;
    if(a<Bottom) a = Bottom;
    if(c>Top)    c = Top;
    if(b < 0 || d < 0 || b > NoPixcels || d > NoPixcels) {
	Error("Out of range in SendEdge",INTERNAL);
	}

    if(a < LastA) Error("Out of order edges",INTERNAL);
    e = GetEdge();
    e->ix = a;
    e->ex = c;
    e->iy = b;
    if(c != a)
	e->dy = ((real) (d-b))/((real) (c-a));
      else
	e->dy = 0.0;
    e->dir = dir;
    if(!((dir == 1) || (dir == -1)))
	Error("bad direction vector in SendEdge",INTERNAL);
    e->poly = poly;
    (poly->refs)++;
    if(!extractor)
	PutQueue(e,&(EdgeQueue[poly->level]));
      else /* if running extractor everything goes on same layer */
	PutQueue(e,&(EdgeQueue[0]));
    }

DupEdge(a,b,c,d,dir,poly)
int a,b,c,d;
PolyDesc *poly;
{
    iedge *e;

    e = GetIEdge();
    e->type = EDGE;
    e->poly = poly;
    (e->poly->refs)++;
    e->dir = dir;
    e->min = a;
    e->x1 = a;	e->y1 = b;
    e->x2 = c;	e->y2 = d;
    Selector(e);
    }
