/*******************************************************************
*                                                                  *
*    File: CIFPLOT/text.c                                          *
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

IMPORT GetList();
FORWARD real TCompare();

Text(xcurrent)
int xcurrent;
{
    TextStruct *p;

    p = (TextStruct *) TextList.Link;
    while(p != NIL && p->xpos-xcurrent < -TextDown-1) {
	p = (TextStruct *) GetList(&TextList);
	p = (TextStruct *) TextList.Link;
	}
    for(; p != NIL; p = (TextStruct *) p->Link) {
	if(p->xpos-xcurrent > TextUp+1) return;
	DrawText(p->str,p->xpos-xcurrent,p->ypos,xcurrent);
	}
    }

real
TCompare(t1,t2)
TextStruct *t1,*t2;
{
    return((float) (t1->xpos - t2->xpos));
    }
