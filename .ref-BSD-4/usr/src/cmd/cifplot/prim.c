/*******************************************************************
*                                                                  *
*    File: CIFPLOT/prim.c                                          *
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

IMPORT GetQueue();
IMPORT PutQueue();
IMPORT PeekList();

FORWARD real ECompare();

AddEdges(xcurrent)
int xcurrent;
/* Add Edges from edge queue to active edge list */
{
    int insert,i,Next;
    register instance *p;
    register edge *e;

    /*
    p = (instance *) PeekHeap();
    */
    /* Next is the raster value of item on top of list */
    /*
    if(p==NIL) Next = Top;
    	  else Next = CONVERT(p->min);
	  */
    Next = xcurrent + 1;
    NextEdge = NextUnAct;
    for(i=0;i<UsedLayers;i++) {
	insert = 0; /* insert is set to 1 if any edge has been inserted */
	while( (EdgeQueue[i].QStart != NIL) &&
		(((edge *) (EdgeQueue[i].QStart))->ix <= xcurrent) ) {
	    insert = 1;
	    /* Update the iy values if not valid */
	    if(Valid[i] != xcurrent) Validate(i,xcurrent);
	    e = (edge *) GetQueue(&(EdgeQueue[i]));
	    /* EdgeEnd[i] indicates when the next edge ends in layer i */
	    EdgeEnd[i] = MIN(EdgeEnd[i],e->ex);
	    PutList(e,&TempList,ECompare);
	    }
	if(insert) {
	    MergeActives(i,&TempList);
	    /* change[i] is set to 1 if anything has changed in layer i */
	    change[i] = 1;
	    /* LastEdge indicates when the next edge ends for all layers */
	    LastEdge = MIN(LastEdge,EdgeEnd[i]);
	    /* Compute where the next edge intersection will occur on layer i */
	    Intersection(i,xcurrent);
	    }
	/* Compute when the next edge will enter the active list for layer i */
	if(EdgeQueue[i].QStart == NIL) EdgeStart[i] = NextUnAct;
	   else
		EdgeStart[i] = ((edge *) (EdgeQueue[i].QStart))->ix;
	/* NextEdge indicates when the next edge will become active */
	NextEdge = MIN(NextEdge,EdgeStart[i]);
	}
    }

RemoveEdges(xcurrent)
int xcurrent;
{
    edge *e,*q;
    int i;

    for(i=0;i<UsedLayers;i++)
	/* For each layer check to see if time to remove edges */
	if(xcurrent >= EdgeEnd[i]) {
	    for(e = (edge *) &(ActiveEdges[i]); e->Link != NIL; )
		if(e->Link->ex <= xcurrent) {
		    /* If time to remove e->Link then unlink it. Record a change */
		    change[i] = 1;
		    q = e->Link;
		    e->Link = e->Link->Link;
		    /* If no more references to the PolyDesc then free it */
		    if(--(q->poly->refs) == 0) FreeDesc(q->poly);
		    FreeEdge(q);
		    }
		 else e=e->Link;
	    }
    }

Sort(xcurrent)
register xcurrent;
{
    register int i;
    
    for(i=0;i<UsedLayers;i++) {
	/* For each layer check to see if an intersection may
	 * have occured */
	if(xcurrent >= EdgeIntersection[i]) {
	    /* Mark that a change has occured and update the y
	     * values in the active list */
	    if(Valid[i] != xcurrent) Validate(i,xcurrent);

	    /* SortActive sets change[i] if anything was swapped */
	    SortActives(i);

	    /* Compute where the next intersection will take place */
	    Intersection(i,xcurrent);
	    }
	}
    }

Validate(i,xcurrent)
register int i;
int xcurrent;
/* Validate updates the y values of the elements in
 * the active list for layer i */
{
    register real diff;
    register edge *e;

    diff = xcurrent - Valid[i];
    for(e=(edge *) ActiveEdges[i].Link; e!=NIL; e=e->Link)
	e->iy += diff*e->dy;
    /* Valid[i] records the last update for layer i */
    Valid[i] = xcurrent;
    }

Intersection(i,xcurrent)
int i,xcurrent;
/* Intersection calculates the next time to edges in the list cross */
{
    register edge *e1,*e2;
    register real diff;		/* diff is the difference from the
				 * estimated next intersection and
				 * xcurrent (the current raster line) */

    e1 = (edge *) ActiveEdges[i].Link;
    if(e1 == NIL || e1->Link == NIL) return;
    /*
    diff = MIN(EdgeIntersection[i],EdgeStart[i]) - xcurrent;
    */
    diff = Top - xcurrent;
    for(e2=e1->Link; e2 != NIL && diff > 1.0;(e1 = e2, e2= e1->Link))
	if( (e1->iy + e1->dy*diff) > (e2->iy + e2->dy*diff) ) {
	    /* Edges cross, compute intersection
	     * diff is reused as a temp but restored at end	*/
	    diff = e2->dy - e1->dy;
	    if(diff != 0.0) diff = (e1->iy - e2->iy)/diff;
	    }
    EdgeIntersection[i] = MAX((int) (xcurrent + diff) - 1,xcurrent);
    NextChange[i] = MIN(EdgeEnd[i],EdgeIntersection[i]);
    NextChange[i] = MIN(NextChange[i],EdgeStart[i]);
    }

int *in;
real *it,*dt;

ScanActives(xcurrent)
register int xcurrent;
{
    register int i;
    register edge *e;

    LastEdge = Top;
    for(i=0; i<UsedLayers;i++) {
	/* If change rescan active list, otherwise redraw from previous
	 * information by calling Draw */
	if(change[i]) {
	    OutputTrap(i,xcurrent);
	    /* Update all the iy's */
	    if(Valid[i] != xcurrent) Validate(i,xcurrent);
	    /* Compute when next edge ends */
	    EdgeEnd[i] = Top;
	    in[i] = 0;

	    for(e= (edge *) ActiveEdges[i].Link; e!=NIL; e=e->Link) {
		if(e->poly->count == 0) {
		    	e->poly->iy = e->iy;
		    	e->poly->dy = e->dy;
		    	if(in[e->poly->level]++ == 0) {
			    it[e->poly->level] = e->iy;
			    dt[e->poly->level] = e->dy;
			    if(extractor)
				OutputExtEdge(e->iy,e->dy,e->poly->level,1);
			    }
		    	}
		if(0 == (e->poly->count += e->dir)) {
		    	if(--in[e->poly->level] == 0) {
			    if(extractor)
				OutputExtEdge(e->iy,e->dy,e->poly->level,0);
			     else
				SendDisplay(it[e->poly->level],
					    dt[e->poly->level],
					    e->iy,e->dy,i);
			    }
		    	}
		EdgeEnd[i] = MIN(EdgeEnd[i],e->ex);
		}
	    if(in[i]) {
		fprintf(stderr,"in[%d] = %d\t",i,in[i]);
		fprintf(stderr,"xcurrent = %d\n",xcurrent);
		for(e=(edge *) ActiveEdges[i].Link;e!=NIL;e=e->Link) {
		    fprintf(stderr,"Edge %x: iy=%f, dy=%f, ix=%d, ex=%d;\n",e,e->iy,e->dy,e->ix,e->ex);
		    fprintf(stderr,"\t\tdir=%d, poly=%x\n",e->dir,e->poly);
		    }
		Error("Bad Count in ScanActives",INTERNAL);
		}
	    /* Reset change */
	    change[i] = 0;
	    NextChange[i] = MIN(EdgeEnd[i],EdgeIntersection[i]);
	    NextChange[i] = MIN(NextChange[i],EdgeStart[i]);
	    FinnishLine(i,xcurrent);
	    }
	LastEdge = MIN(LastEdge,EdgeEnd[i]);
	}
    }

InitScan() {
    int i;
    in = (int *) alloc(UsedLayers*sizeof(int));
    it = (real *) alloc(UsedLayers*sizeof(real));
    dt = (real *) alloc(UsedLayers*sizeof(real));
    for(i=0; i<UsedLayers; i++) {
	in[i] = 0;
	}
    }

/*
DTest(i,xcurrent)
int i,xcurrent;
{
    do {
	Draw(i,xcurrent);
	xcurrent++;
	} while(xcurrent < NextChange[i]);
    }
    */

real ECompare(a,b)
edge *a,*b;
{
    return(a->iy - b->iy);
    }
