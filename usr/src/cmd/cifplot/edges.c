/*******************************************************************
*                                                                  *
*    File: CIFPLOT/edges.c                                         *
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
IMPORT Command *MakeFlash();
IMPORT Command *MakeBox();
IMPORT transform *MatrixMult();
IMPORT transform *Translate();
IMPORT real ICompare();
IMPORT real TCompare();
IMPORT StartEdgePath();
IMPORT iedge *NextEdgePath();
IMPORT iedge *EndEdgePath();

FORWARD iedge *MakeEdge();
FORWARD PolyDesc *MakeDesc();
FORWARD instance *MakeItem();

Instanciate(c,trans,n)
Command *c;
transform *trans;
int n;
{
    instance *i;
    Command *p;
    real x,y,xmin,ymin;

    if(c->type != SYMBOL)
	Error("Tried to Instanciate a Non-Symbol",INTERNAL);
    if(depth == 0 || n <= depth) {
    	for(p=c->Ctype.Symbl.CStart; p!=NIL; p=p->CLink) {
	    i = MakeItem(p,trans,n+1);
	    Selector(i);
	    }
        if(symbox) DrawBBox(&(c->CBBox),trans);
        if(*(c->Ctype.Symbl.SName) == '\0') return;
        xmin = c->CBBox.xmin;	ymin = c->CBBox.ymin;
        Trans(&xmin,&ymin,trans);
        
        x = c->CBBox.xmax;	y = c->CBBox.ymin;
        Trans(&x,&y,trans);
        if(x+y < xmin+ymin) { ymin = y; xmin = x; }

        x = c->CBBox.xmax;	y = c->CBBox.ymax;
        Trans(&x,&y,trans);
        if(x+y < xmin+ymin) { ymin = y; xmin = x; }

        x = c->CBBox.xmin;	y = c->CBBox.ymax;
        Trans(&x,&y,trans);
        if(x+y < xmin+ymin) { ymin = y; xmin = x; }

        ClipText(c->Ctype.Symbl.SName,xmin,ymin,'S');
	}
      else {	/* Don't instanciate this symbol; just draw box and name*/
	DrawBBox(&(c->CBBox),trans);
        if(*(c->Ctype.Symbl.SName) == '\0') return;
        xmin = c->CBBox.xmin;	ymin = c->CBBox.ymin;
        Trans(&xmin,&ymin,trans);
        x = c->CBBox.xmax;	y = c->CBBox.ymax;
        Trans(&x,&y,trans);
	x = (xmin+x)/2;		y = (ymin+y)/2;
        ClipText(c->Ctype.Symbl.SName,x,y,'C');
	}
    }

Activate(i)
instance *i;
{
    switch(i->type) {
	case CALL:
	    {	transform *t;
		t = MatrixMult(i->item->Ctype.Call.trans,i->trans);
		Instanciate(i->item->Ctype.Call.CSymb,t,i->depth);
		}
		FreeItem(i);
		break;
	case ARRAY:
	    {   int j,k;
		point pt;
		transform *trans,*t;
		instance *inst;

		pt.x = 0.0;
		for(j=0; j<i->item->Ctype.Array.Am; j++) {
		    pt.y = 0.0;
		    for(k=0; k<i->item->Ctype.Array.An; k++) {
			trans = Translate(&pt,ident);
			t = MatrixMult(trans,i->trans);
			FreeTransform(trans);
			inst = MakeItem(i->item->Ctype.Array.ACom,t,i->depth);
			Selector(inst);
			pt.y += i->item->Ctype.Array.Ady;
			}
		    pt.x += i->item->Ctype.Array.Adx;
		    }
		}
		break;
	case POLYGON:
	    {	PolyDesc *poly;
		PointList *path;
		iedge *e;
		point *pt;

		poly = MakeDesc(i->item);
		path = i->item->Ctype.Path;
		pt = &(path->pt);
		StartEdgePath(pt->x,pt->y,i->trans,poly);
		/* Assertion: there are at least two points
		 * in every polygon	*/
		for(path = path->PLink; path != NIL; path = path->PLink){
			pt = &(path->pt);
			e = NextEdgePath(pt->x,pt->y);
			Selector(e);
			}
		e = EndEdgePath();
		Selector(e);
		}
		FreeItem(i);
		break;
	case BOX:
	    {	PolyDesc *poly;
		point *d;
		real ly,lx,wx,wy,cx,cy,c;
		iedge *e;
		
		poly = MakeDesc(i->item);
		d = &(i->item->Ctype.Box.bdirect);
		c = sqrt(d->x*d->x + d->y*d->y);
		cx = i->item->Ctype.Box.bcenter.x;
		cy = i->item->Ctype.Box.bcenter.y;
		lx = (d->x*i->item->Ctype.Box.blength)/(c*2.0);
		ly = (d->y*i->item->Ctype.Box.blength)/(c*2.0);
		wx = -(d->y*i->item->Ctype.Box.bwidth)/(c*2.0);
		wy = (d->x*i->item->Ctype.Box.bwidth)/(c*2.0);
		StartEdgePath(cx-lx-wx,cy-ly-wy,i->trans,poly);
		e = NextEdgePath(cx-lx+wx,cy-ly+wy);
		Selector(e);
		e = NextEdgePath(cx+lx+wx,cy+ly+wy);
		Selector(e);
		e = NextEdgePath(cx+lx-wx,cy+ly-wy);
		Selector(e);
		e = EndEdgePath();
		Selector(e);
		/*
		e = MakeEdge(cx-lx-wx,cy-ly-wy,cx-lx+wx,cy-ly+wy,i->trans,poly);
		Selector(e);
		e = MakeEdge(cx-lx+wx,cy-ly+wy,cx+lx+wx,cy+ly+wy,i->trans,poly);
		Selector(e);
		e = MakeEdge(cx+lx+wx,cy+ly+wy,cx+lx-wx,cy+ly-wy,i->trans,poly);
		Selector(e);
		e = MakeEdge(cx+lx-wx,cy+ly-wy,cx-lx-wx,cy-ly-wy,i->trans,poly);
		Selector(e);
		*/
		FreeItem(i);
		}
		break;
	case FLASH:
	    {	PolyDesc *poly;

		poly = MakeDesc(i->item);
		MakeNgon(circle,&(i->item->Ctype.Flash.fcenter),i->item->Ctype.Flash.fdia/2.0,i->trans,poly);
		}
		FreeItem(i);
		break;
	case WIRE:
	    {	real width;
		PointList *p1,*p2;
		Command *com;
		instance *inst;
		point *dir,*cen;
		real x,y;
		int level;

		if(i->item->Ctype.Wire.WIns == NIL) {
		    width = i->item->Ctype.Wire.WWidth;
		    level = i->item->level;
		    p1 = i->item->Ctype.Wire.WPath;
		    com = MakeFlash(width,&(p1->pt));
		    com->level = level;
		    com->CLink = NIL;
		    i->item->Ctype.Wire.WIns = com;
		    inst = MakeItem(com,i->trans,i->depth);
		    Selector(inst);
		    for((p2=p1,p1=p1->PLink);p1 != NIL;(p2=p1,p1=p1->PLink)) {
			x = p1->pt.x - p2->pt.x; y = p1->pt.y - p2->pt.y;
			com = MakeFlash(width,&(p1->pt));
			com->level = level;
			com->CLink = i->item->Ctype.Wire.WIns;
			i->item->Ctype.Wire.WIns = com;
			inst = MakeItem(com,i->trans,i->depth);
			Selector(inst);

			dir = MakePoint(x,y);
			cen = MakePoint((p1->pt.x + p2->pt.x)/2.0,(p1->pt.y + p2->pt.y)/2.0);
			com = MakeBox(sqrt(x*x+y*y),width,cen,dir);
			com->level = level;
			com->CLink = i->item->Ctype.Wire.WIns;
			i->item->Ctype.Wire.WIns = com;
			inst = MakeItem(com,i->trans,i->depth);
			Selector(inst);
			Free(cen); Free(dir);
			}
		    }
		  else {
		    for(com=i->item->Ctype.Wire.WIns;com!=NIL;com=com->CLink) {
			inst = MakeItem(com,i->trans,i->depth);
			Selector(inst);
			}
		    }
		}
		FreeItem(i);
		break;
	case EDGE:
		Clip(i);
		if(--(((iedge *) i)->poly->refs) == 0)
			FreeDesc(((iedge *) i)->poly);
		FreeIEdge(i);
		break;
	case POINTNAME:
	    {   real x,y;
		x = i->item->Ctype.PointName.loc.x;
		y = i->item->Ctype.PointName.loc.y;
		Trans(&x,&y,i->trans);
		if(!extractor)
		    ClipText(i->item->Ctype.PointName.Name,x,y,'T');
		  else
		    OutputExtPoint(x,y,i->item->Ctype.PointName.Name,
				    i->item->level);
		}
		break;
	case STEXT:
	    {   real x,y;
		Command *icom;
		icom = (Command *) i;

		x = icom->Ctype.PointName.loc.x;
		y = icom->Ctype.PointName.loc.y;
		ClipText(icom->Ctype.PointName.Name,x,y,'T');
		FreeStif(icom);
		}
		break;
	case VECTOR:
	    {   Command *icom;
		PointList *p1,*p2;
		icom = (Command *) i;

		p1 = icom->Ctype.Path;
		if(p1 == NIL) Error("Vector has null path in edge",INTERNAL);
		for((p2=p1,p1=p1->PLink); p1 != NIL;(p2=p1,p1=p1->PLink)) {
		    MakeLine(p1->pt.x,p1->pt.y,p2->pt.x,p2->pt.y,ident);
		    }
		FreeStif(icom);
		}
		break;
	case SPOLYGON:
	    {	PolyDesc *poly;
		PointList *path;
		iedge *e;
		point *pt;
		Command *icom;
		icom = (Command *) i;
		i = (instance *) 0xffffffff; /* debugging */

		poly = MakeDesc(icom);
		path = icom->Ctype.Path;
		pt = &(path->pt);
		StartEdgePath(pt->x,pt->y,ident,poly);
		/* Assertion: there are at least two points
		 * in every polygon	*/
		for(path = path->PLink; path != NIL; path = path->PLink){
			pt = &(path->pt);
			e = NextEdgePath(pt->x,pt->y);
			Selector(e);
			}
		e = EndEdgePath();
		Selector(e);
		FreeStif(icom);
		}
		break;
	case SFLASH:
	    {	PolyDesc *poly;
		Command *icom;
		icom = (Command *) i;
		i = (instance *) 0xffffffff; /* debugging */

		poly = MakeDesc(icom);
		MakeNgon(circle,&(icom->Ctype.Flash.fcenter),icom->Ctype.Flash.fdia/2.0,ident,poly);
		FreeStif(icom);
		}
		break;
	case SWIRE:
	    {	real width;
		PointList *p1,*p2;
		Command *ncom;
		instance *inst;
		point *dir,*cen;
		real x,y;
		int level;
		Command *icom;
		icom = (Command *) i;
		i = (instance *) 0xffffffff; /* debugging */

		width = icom->Ctype.Wire.WWidth;
		level = icom->level;
		p1 = icom->Ctype.Wire.WPath;
		ncom = MakeFlash(width,&(p1->pt));
		ncom->level = level;
		ncom->CLink = NIL;
		inst = MakeItem(ncom,ident,0);
		Selector(inst);
		for((p2=p1,p1=p1->PLink);p1 != NIL;(p2=p1,p1=p1->PLink)) {
		    x = p1->pt.x - p2->pt.x; y = p1->pt.y - p2->pt.y;
		    ncom = MakeFlash(width,&(p1->pt));
		    ncom->level = level;
		    inst = MakeItem(ncom,ident,0);
		    Selector(inst);

		    dir = MakePoint(x,y);
		    cen = MakePoint((p1->pt.x + p2->pt.x)/2.0,(p1->pt.y + p2->pt.y)/2.0);
		    ncom = MakeBox(sqrt(x*x+y*y),width,cen,dir);
		    ncom->level = level;
		    inst = MakeItem(ncom,ident,0);
		    Selector(inst);
		    Free(cen); Free(dir);
		    }
		FreeStif(icom);
		}
		break;
	case SCALL:
		StifCall(i);
		FreeStif(i);
		break;
	default:
		Error("Tried to Activate Unknown type",INTERNAL);
	}
    }

instance *
MakeItem(c,trans,n)
Command *c;
transform *trans;
int n;
{
    instance *i;

    i = GetItem();
    i->type = c->type;
    i->Link = NIL;
    i->trans = trans;
    trans->refs++;
    i->item = c;
    i->depth = n;
    return(i);
    }
		
iedge *
MakeEdge(rx1,ry1,rx2,ry2,trans,poly)
real rx1,rx2,ry1,ry2;
transform *trans;
PolyDesc *poly;
{
    iedge *e;
    real t;
    
    e = GetIEdge();
    e->type = EDGE;
    e->poly = poly;
    (poly->refs)++;
    Trans(&rx1,&ry1,trans);
    Trans(&rx2,&ry2,trans);
    if(rx1 <= rx2)  e->dir = 1;
	else {	e->dir = -1;
		SWAP(rx1,rx2,t);
		SWAP(ry1,ry2,t);
		}
    /* Now rx1 <= rx2 and the direction is set */
    /* Now convert from CIF units to plotter units */
    e->x1 = CONVERT(rx1); e->y1 = CONVERT(ry1);
    e->x2 = CONVERT(rx2); e->y2 = CONVERT(ry2);
    e->min = CONVERT(rx1);
    return(e);
    }

PolyDesc *
MakeDesc(c)
Command *c;
{
    PolyDesc *p;

    p = GetDesc();
    if(c != NIL)
    	p->level = c->level;
     else
	p->level = 0;
    p->count = 0;
    p->refs = 0;
    return(p);
    }

/*
FreeInstance(i)
instance *i;
{
    if(--(i->trans->refs) == 0) FreeTransform(i->trans);
    FreeItem(i);
    }
    */

