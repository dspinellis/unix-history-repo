/*******************************************************************
*                                                                  *
*    File: CIFPLOT/edgepath.c                                      *
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


static int xstart,ystart,xlast,ylast;
static PolyDesc *poly;
static transform *trans;

StartEdgePath(x,y,itrans,ipoly)
real x,y;
transform *itrans;
PolyDesc *ipoly;
{
    poly = ipoly;
    trans = itrans;
    Trans(&x,&y,trans);
    xstart = xlast = CONVERT(x);
    ystart = ylast = CONVERT(y);
    return;
    }

iedge *NextEdgePath(x,y)
real x,y;
{
    iedge *e;
    int t;
    int ix1,ix2,iy1,iy2;

    Trans(&x,&y,trans);
    ix1 = ix2 = CONVERT(x);	iy1 = iy2 = CONVERT(y);

    e = GetIEdge();
    e->type = EDGE;
    e->poly = poly;
    (poly->refs)++;
    if(xlast <= ix1) e->dir = 1;
      else {
	e->dir = -1;
	SWAP(xlast,ix1,t);
	SWAP(ylast,iy1,t);
	}
    /* xlast <= x */
    e->min = e->x1 = xlast;	e->y1 = ylast;
    e->x2 = ix1;		e->y2 = iy1;
    xlast = ix2;		ylast = iy2;
    return(e);
    }

iedge *EndEdgePath()
{
    iedge *e;
    int t;

    e = GetIEdge();
    e->type = EDGE;
    e->poly = poly;
    (poly->refs)++;
    if(xlast <= xstart) e->dir = 1;
      else {
	e->dir = -1;
	SWAP(xlast,xstart,t);
	SWAP(ylast,ystart,t);
	}
    e->min = e->x1 = xlast;	e->y1 = ylast;
    e->x2 = xstart;		e->y2 = ystart;
    return(e);
    }
