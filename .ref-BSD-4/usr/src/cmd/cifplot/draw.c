/*******************************************************************
*                                                                  *
*    File: CIFPLOT/draw.c                                          *
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

IMPORT nedge *AllocHolder();
IMPORT PutQueue();
IMPORT GetQueue();
IMPORT AddLine();
IMPORT Fill();

SendDisplay(a,da,b,db,l)
register int l;
real a,b,da,db;
/* SendDisplay sends the y positions a & b to be displayed on plot
 * Each raster line the value of a & b change by da & db */
{
    if(a > b) Error("Edge is backwards in SendEdge",INTERNAL);
    if(a < 0) Error("bad value in SendEdge",INTERNAL);
    if(a == b && da == db && l != 0) return;  /* Ignore 0 width lines */
    if(EdgeHolder[l] != NIL)
	if(a<EdgeHolder[l]->end || (a==EdgeHolder[l]->end &&  da <= EdgeHolder[l]->dend)) {
	    /* If this run overlaps the last run combine them 
	     * and return
	     * if not send out last run and create new run
	     * if equal do not combine since dy may diverge */
	    EdgeHolder[l]->end = b;
	    EdgeHolder[l]->dend = db;
	    return;
	    }
	  else
	     PutQueue(EdgeHolder[l],&(NEdgeQueue[l]));
    EdgeHolder[l] = AllocHolder();
    EdgeHolder[l]->start = a;
    EdgeHolder[l]->end = b;
    EdgeHolder[l]->dstart = da;
    EdgeHolder[l]->dend = db;
    }

FinnishLine(l,xcurrent)
int l;
/* Causes last holder to be put on queue, calls AddLine */
{
    if(EdgeHolder[l] != NIL)
    	PutQueue(EdgeHolder[l],&(NEdgeQueue[l]));
    EdgeHolder[l] = NIL;
    AddLine(l,xcurrent);
    }

/*
Draw(i,xcurrent)
register int i,xcurrent;
{
    register nedge *e;

    return;
    /* Fill between edges; compute outlines if outline is true
     * and the pattern is not black. (If the pattern is black
     * outlines do not appear anyway.)	*/
    /*
    if(outline && Pats[i][ModCount] != 0xFFFFFFFF)
	for(e=(nedge *) NecEdgeQueue[i].QStart; e!=NIL; e=e->Link) {
	    if(e->dstart > 0.0)
		Fill(xcurrent,(int) e->start,(int) (e->start + e->dstart),0xFFFFFFFF);
	      else
		Fill(xcurrent,(int) (e->start + e->dstart),(int) e->start,0xFFFFFFFF);
	    if(e->dend > 0.0)
		Fill(xcurrent,(int) e->end+1,(int) (e->end + e->dend)+1,0xFFFFFFFF);
	      else
		Fill(xcurrent,(int) (e->end + e->dend)+1,(int) e->end+1,0xFFFFFFFF);
	    e->start += e->dstart; e->end += e->dend;
	    Fill(xcurrent,(int) e->start, (int) e->end,Pats[i][ModCount]);
	    }
      else
	for(e=(nedge *) NecEdgeQueue[i].QStart; e!=NIL; e=e->Link)
	    Fill(xcurrent,(int) (e->start+= e->dstart), (int) (e->end+= e->dend)+1,Pats[i][ModCount]);
    }
    */


OutputTrap(layer,xcurrent)
int layer,xcurrent;
{
    register nedge *e;
    register real diff;
    if(extractor) {
	ExtractorOutput(xcurrent);
	return;
	}
    diff = xcurrent - xprev[layer] - 1;
    for(e=(nedge *) NecEdgeQueue[layer].QStart; e!=NIL; e=e->Link) {
	FillTrap(e->start,e->end,e->dstart,e->dend,xprev[layer],xcurrent,layer);
	e->start += e->dstart * diff;	e->end += e->dend * diff;
	}
    xprev[layer] = xcurrent;
    }
