/*******************************************************************
*                                                                  *
*    File: CIFPLOT/compare.c                                       *
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

IMPORT MoveQueue();
IMPORT CatQueue();

#define GETA	if(p1 != NIL) {a_start= p1->start; a_end= p1->end;\
					p1= p1->Link; }\
				else {a_start = a_end = INFINITY;}\
		if(a_start > a_end) \
				Error("a_start > a_end",INTERNAL);

#define GETB	if(p2 != NIL) {b_start= p2->start; b_end= p2->end;\
					p2= p2->Link; }\
				else {b_start = b_end = INFINITY;}\
		if(b_start > b_end) \
				Error("b_start > b_end",INTERNAL);

AddLine(i,xcurrent)
int i,xcurrent;
{
    nedge *p1,*p2;
    int a_start,a_end,b_start,b_end;

    /* Don't run if just finishing buffer */
    if(!finishBuf) {
	p2 = (nedge *) NEdgeQueue[i].QStart;
	if(outline && !(extractor)) {
	    p1 = (nedge *) NecEdgeQueue[i].QStart;
	    GETA; GETB;
	    while(a_start != INFINITY || b_start != INFINITY) {
		if(a_start <= b_start)
		    if(a_end < b_start) {
			Fill(xcurrent,(int) a_start,(int) a_end,0xFFFFFFFF);
			GETA;
			continue;
			}
		      else {
			Fill(xcurrent,(int) a_start,(int) b_start,0xFFFFFFFF);
			a_start = b_start;
			}
		  else
		    if(b_end < a_start) {
			Fill(xcurrent,(int) b_start,(int) b_end,0xFFFFFFFF);
			GETB;
			continue;
			}
		      else {
			Fill(xcurrent,(int) b_start,(int) a_start,0xFFFFFFFF);
			b_start = a_start;
			}
		if(a_start == b_start)
		    if(a_end < b_end) {
			b_start = a_end;
			GETA;
			continue;
			}
		      else {
			a_start = b_end;
			GETB;
			continue;
			}
		}
	    }
	CatQueue(&FreeHolders,&(NecEdgeQueue[i]));
	MoveQueue(&(NecEdgeQueue[i]),&(NEdgeQueue[i]));
	InitQueue(&(NEdgeQueue[i]));
	}
    }
