/*******************************************************************
*                                                                  *
*    File: CIFPLOT/line.c                                          *
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
#include <math.h>
#include "alloc.h"

IMPORT point *MakePoint();
IMPORT point *TransPt();
IMPORT real ICompare();
IMPORT StartEdgePath();
IMPORT iedge *NextEdgePath();
IMPORT iedge *EndEdgePath();
IMPORT PolyDesc *MakeDesc();
IMPORT char *Concat();

#define pi 3.1415926535

MakeLine(rx1,ry1,rx2,ry2,trans)
real rx1,rx2,ry1,ry2;
transform *trans;
{
    int i;
    iedge *e1,*e2;
    real t;

    Trans(&rx1,&ry1,trans);
    Trans(&rx2,&ry2,trans);
    if(rx1 > rx2)  {  SWAP(rx1,rx2,t); SWAP(ry1,ry2,t);}
    /* Now rx1 <= rx2 */

    e1 = GetIEdge();
    e1->type = EDGE;
    e1->poly = MakeDesc(NIL);
    (e1->poly->refs)++;

    e2 = GetIEdge();
    e2->type = EDGE;
    e2->poly = e1->poly;
    (e1->poly->refs)++;

    /* Now convert from CIF units to plotter units */
    e1->x1 = e2->x1 = CONVERT(rx1); e1->y1 = e2->y1 = CONVERT(ry1);
    e1->x2 = e2->x2 = CONVERT(rx2); e1->y2 = e2->y2 = CONVERT(ry2);
    e1->min = e1->x1;
    e2->min = e1->x1;

    e1->dir = 1; e2->dir = -1;
    if(e2->x1 == e2->x2) { SWAP(e2->y1,e2->y2,i); }
    PutUnAct(e1);
    PutUnAct(e2);
    }

DrawBBox(bb,trans)
struct BBox *bb;
transform *trans;
{

    MakeLine((real) bb->xmin,(real) bb->ymin,(real) bb->xmax,(real) bb->ymin,trans);
    MakeLine((real) bb->xmax,(real) bb->ymin,(real) bb->xmax,(real) bb->ymax,trans);
    MakeLine((real) bb->xmax,(real) bb->ymax,(real) bb->xmin,(real) bb->ymax,trans);
    MakeLine((real) bb->xmin,(real) bb->ymax,(real) bb->xmin,(real) bb->ymin,trans);
    }

MakeNgon(n,center,radius,trans,poly)
int n;
point *center;
real radius;
transform *trans;
PolyDesc *poly;
{
    real d;
    int i;
    real x,y;
    iedge *e;

    d = -(pi/((real) n));
    radius = radius/cos(-d);
    x = radius*sin(d)+center->x;  y = radius*cos(d)+center->y;

    StartEdgePath(x,y,trans,poly);
    for((i=1,d += (2.0*pi)/((real) n));i<n;(i++,d += (2.0*pi)/((real) n))) {
    	x = radius*sin(d)+center->x;  y = radius*cos(d)+center->y;
	e = NextEdgePath(x,y);
	Selector(e);
	}
    e = EndEdgePath();
    Selector(e);
    }

Grid()
{
	double x,y;
	char s[512],*str;
	if(grid != 0) {
	    /* Draw vertical grid lines */
	    x = ((int) (Window.xmin/grid)) * grid;
	    while(x<Window.xmax) {
		if(Window.xmin < x) {
		    MakeLine(x,Window.ymin,x,Window.ymax,ident);
		    sprintf(s,"%d",(int) x);
		    str = Concat(s,0);
		    ClipText(str,x,Window.ymax,'r');
		    }
		x += grid;
		}
	    /* Draw horizontal grid lines */
	    y = ((int) (Window.ymin/grid)) * grid;
	    while(y<Window.ymax) {
		if(Window.ymin < y) {
		    MakeLine(Window.xmin,y,Window.xmax,y,ident);
		    sprintf(s,"%d",(int) y);
		    str = Concat(s,0);
		    ClipText(str,Window.xmax,y,'r');
		    }
		y += grid;
		}
	    }
    }

