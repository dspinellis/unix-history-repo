/*******************************************************************
*                                                                  *
*    File: CIFPLOT/select.c                                        *
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

IMPORT transform *MatrixMult();
IMPORT Trans();
IMPORT real TCompare();
IMPORT struct LCell *GetLayer();
FORWARD real ICompare();

Selector(i)
iedge *i;
{
    instance *item;
    transform *trans;
    real x,y;
    TextStruct *t;
    Command *c;
    struct BBox bb;
    struct LCell *layer;

    switch(i->type) {
	case EDGE:
		if(i->x1 == i->x2) {
			--(i->poly->refs);
			FreeIEdge(i);
			return;
			}
		break;
	case TEXT:
    		item = (instance *) i;
		trans = MatrixMult(item->item->Ctype.Text.TTrans,item->trans);
		x = 0.0; y = 0.0;
		Trans(&x,&y,trans);
		FreeTransform(trans);
		ClipText(item->item->Ctype.Text.TString,x,y,item->item->Ctype.Text.TLoc);
		return;
	case SPOLYGON:
	case SFLASH:
	case SWIRE:
	case SCALL:
	case STEXT:
	case VECTOR:
	    {   Command *icom;
		icom = (Command *) i;
		if(icom->CBBox.xmax < Window.xmin || Window.xmax < icom->CBBox.xmin ||
		   icom->CBBox.ymax < Window.ymin || Window.ymax < icom->CBBox.ymin) {
			Free(icom);
			return;
			}
		icom->min = CONVERT(icom->CBBox.xmin-1.0);
		}
		break;
	case POLYGON:
	case WIRE:
	case BOX:
	case POINTNAME:
		item = (instance *) i;
		layer = GetLayer(item->item->level);
		if(layer != NIL && !(layer->visible)) return;
		/* else continue through next case */
	default:
		item = (instance *) i;
		c = item->item;
		ZeroBBox(&bb);
		BBoxTransform(&bb,&(c->CBBox),item->trans);
		if(bb.xmax < Window.xmin || Window.xmax < bb.xmin ||
		   bb.ymax < Window.ymin || Window.ymax < bb.ymin) {
			FreeItem(item);
			return;
			}
		item->min = CONVERT(bb.xmin-1.0);
		break;
	}
    PutUnAct(i);
    }

real
ICompare(i,j)
instance *i,*j;
/* ICompare(i,j) <= 0 iff i->min <= j->min iff i->min - j->min <= 0 */
{
    return((real) (i->min - j->min));
    }
