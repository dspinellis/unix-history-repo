/*******************************************************************
*                                                                  *
*    File: CIFPLOT/bbox.c                                          *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"
#include "alloc.h"

IMPORT transform *MakeTransform();
IMPORT transform *Rotate();
IMPORT transform *Translate();
IMPORT point *MakePoint();
IMPORT point *TransPt();
IMPORT double cos();

ZeroBBox(bbox)
struct BBox *bbox;
/* Set bbox to the smallest possible value */
{
    bbox->xmin = bbox->ymin = INFINITY;
    bbox->xmax = bbox->ymax = -INFINITY;
    }

ClearBBox(bbox)
struct BBox *bbox;
/* set all values to 0  */
{
    bbox->xmin = bbox->ymin = 0;
    bbox->xmax = bbox->ymax = 0;
    }

MakeBBox(bbox,com)
Command *com;
struct BBox *bbox;
/* The bounding box for the Command 'com' is computed and
 * stored in the location pointed to by 'bbox'	*/
{
    Command *q;
    PointList *p;
    point pt,*ptr;
    transform *t,*t1;
    struct BBox bb;
    
    ZeroBBox(bbox);
    switch(com->type) {
	case POINTNAME:
		CompPtBBox(bbox,&(com->Ctype.PointName.loc));
		break;
	case TEXT:
		break;
	case BOX:
		bb.xmin = com->Ctype.Box.bcenter.x
					- com->Ctype.Box.blength/2.0;
		bb.xmax = com->Ctype.Box.bcenter.x
					+ com->Ctype.Box.blength/2.0;
		bb.ymin = com->Ctype.Box.bcenter.y
					- com->Ctype.Box.bwidth/2.0;
		bb.ymax = com->Ctype.Box.bcenter.y
					+ com->Ctype.Box.bwidth/2.0;
		t = MakeTransform();
		t1 = Translate(&(com->Ctype.Box.bcenter),t);
		FreeTransform(t);
		t = Rotate(&(com->Ctype.Box.bdirect),t1);
		FreeTransform(t1);
		ptr = MakePoint(-(com->Ctype.Box.bcenter.x),-(com->Ctype.Box.bcenter.y));
		t1 = Translate(ptr,t);
		Free(ptr); FreeTransform(t);
		BBoxTransform(bbox,&bb,t1);
		FreeTransform(t1);
		break;
	case FLASH:
	    {
		real radius;
		radius = com->Ctype.Flash.fdia/2.0;
		if(circle != 0)
		    radius = radius/cos(3.1415926535/(real) circle);
		bbox->xmin = com->Ctype.Flash.fcenter.x
					- radius;
		bbox->xmax = com->Ctype.Flash.fcenter.x
					+ radius;
		bbox->ymin = com->Ctype.Flash.fcenter.y
					- radius;
		bbox->ymax = com->Ctype.Flash.fcenter.y
					+ radius;
		}
		break;
	case POLYGON:
		/* The bounding box must surround every point in the polygon */
		for(p=com->Ctype.Path;p != NIL;p=p->PLink) 
			CompPtBBox(bbox,&(p->pt));
		break;
	case WIRE:
	    {
		real scale;
		if(circle == 0)
		    scale = 1.0;
		  else
		    scale = 1.0/cos(3.1415926535/(real) circle);
		/* The bounding box surrounds the wire */
		for(p=com->Ctype.Wire.WPath;p != NIL;p=p->PLink) 
			CompPtBBox(bbox,&(p->pt));
		bbox->xmin -= scale*com->Ctype.Wire.WWidth/2.0;
		bbox->ymin -= scale*com->Ctype.Wire.WWidth/2.0;
		bbox->xmax += scale*com->Ctype.Wire.WWidth/2.0;
		bbox->ymax += scale*com->Ctype.Wire.WWidth/2.0;
		}
		break;
	case SYMBOL:
		/* The bounding box must surround every command in the symbol */
		for(q=com->Ctype.Symbl.CStart;q != NIL;q=q->CLink){
			CompBBox(bbox,&(q->CBBox));
			}
		break;
	case CALL:
		if(com->Ctype.Call.CSymb == NIL) break;
		/* The bounding box must contain all the corners of the
		 * symbol's bounding box after it has been transformed */
		BBoxTransform(bbox,&(com->Ctype.Call.CSymb->CBBox),
					com->Ctype.Call.trans);
		break;
	case ARRAY:
		q = com->Ctype.Array.ACom;
		MakeBBox(&(q->CBBox),q);
		MakeBBox(&(com->CBBox),q);
		pt.x = (com->Ctype.Array.Adx < 0) ?
			com->CBBox.xmin + com->Ctype.Array.Adx*(com->Ctype.Array.Am-1):
			com->CBBox.xmax + com->Ctype.Array.Adx*(com->Ctype.Array.Am-1);
		pt.y = (com->Ctype.Array.Ady < 0) ?
			com->CBBox.ymin + com->Ctype.Array.Ady*(com->Ctype.Array.An-1):
			com->CBBox.ymax + com->Ctype.Array.Ady*(com->Ctype.Array.An-1);
		CompPtBBox(&(com->CBBox),&pt);
		bbox->xmin = com->CBBox.xmin;
		bbox->xmax = com->CBBox.xmax;
		bbox->ymin = com->CBBox.ymin;
		bbox->ymax = com->CBBox.ymax;
		break;
	default:
		Error("Attempted to make bounding box for inappropriate command",INTERNAL);
	}
    }

BBoxTransform(bb,sbb,trans)
struct BBox *bb;
struct BBox *sbb;
transform *trans;
/* Transform the bounding box 'sbb' by transform 'trans' and expand
 * 'bb' till it contains the transformed 'sbb' bounding box.  */
{
    real x,y;

    /* For every corner of bounding box 'sbb', transform it and
     * make 'bb' contain that transformed corner	*/
    x = sbb->xmin;	y = sbb->ymin;
    Trans(&x,&y,trans);
    CompNumBBox(bb,x,y);

    x = sbb->xmax;	y = sbb->ymin;
    Trans(&x,&y,trans);
    CompNumBBox(bb,x,y);

    x = sbb->xmax;	y = sbb->ymax;
    Trans(&x,&y,trans);
    CompNumBBox(bb,x,y);

    x = sbb->xmin;	y = sbb->ymax;
    Trans(&x,&y,trans);
    CompNumBBox(bb,x,y);

    }

CompBBox(bb1,bb2)
struct BBox *bb1,*bb2;
/* Expand bb1 so that it encloses bb2 */
{
    if (bb1->xmin > bb2->xmin)
	bb1->xmin = bb2->xmin;
    if (bb1->ymin > bb2->ymin)
	bb1->ymin = bb2->ymin;
    if (bb1->xmax < bb2->xmax)
	bb1->xmax = bb2->xmax;
    if (bb1->ymax < bb2->ymax)
	bb1->ymax = bb2->ymax;
    }

CompPtBBox(bb,pt)
struct BBox *bb;
point *pt;
/* Expand bb so that it encloses pt */
{
    if (bb->xmin > pt->x)
	bb->xmin = pt->x;
    if (bb->ymin > pt->y)
	bb->ymin = pt->y;
    if (bb->xmax < pt->x)
	bb->xmax = pt->x;
    if (bb->ymax < pt->y)
	bb->ymax = pt->y;
    }

CompNumBBox(bb,x,y)
struct BBox *bb;
real x,y;
/* Expand bb so that in encloses (x,y) */
{
    if (bb->xmin > x)
	bb->xmin = x;
    if (bb->ymin > y)
	bb->ymin = y;
    if (bb->xmax < x)
	bb->xmax = x;
    if (bb->ymax < y)
	bb->ymax = y;
    }
    
