/*******************************************************************
*                                                                  *
*    File: CIFPLOT/heap.c                                          *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "structs.h"
#include "out_structs.h"

#define INITHSIZE	256
#define FATHER(a)	((a)-1)/2
#define LEFTSON(a)	((a)+1)*2 - 1
#define RIGHTSON(a)	((a)+1)*2

#define COMPARE(x,s)	h.h[x]->min - h.h[s]->min
#define LESSEQUAL(x,s)	h.h[x]->min <= h.h[s]->min
#define GREATERTHAN(x,s)	h.h[x]->min > h.h[s]->min
#define SWITCH(x,y)	{ HTemp = h.h[x]; h.h[x] = h.h[y]; h.h[y] = HTemp; }

instance *HTemp;

typedef struct temp723 {
	instance **h;
	int size,n;
	} Heap;
  
int UnActMark = -INFINITY;

Heap h;

InitHeap()
/* Allocate space and initalize pointers */
{
   h.h = (instance **) alloc(INITHSIZE*sizeof(instance *));
   h.n = 0;
   h.size = INITHSIZE;
   }

PutHeap(v)
instance *v;
/* Put what v points to into a heap where f is a function
   that compares values. */
{
    register int a,f;

    if(h.n == h.size) {
	h.size *= 2;
	h.h = (instance **) expand(h.h,h.size*sizeof(instance *));
	}
    h.h[h.n] = v;
    a = h.n++;
    f = FATHER(a);
    while(a > 0  && GREATERTHAN(f,a)) {
	SWITCH(f,a);
	a = f;
    	f = FATHER(a);
	}
    }

instance *
PeekHeap()
{
    if(h.n == 0) return(NIL);
    return(h.h[0]);
    }

instance *
GetHeap()
{
    register instance *hold;

    if(h.n != 0) {
    	hold = h.h[0];
	--h.n;
    	SWITCH(0,h.n);
    	SortHeap(0);
    	return(hold);
	}
     else
	return(NIL);
    }

SortHeap(a)
register int a;
{
    register int r,l;

    while(1) {
    	if((l=LEFTSON(a)) >= h.n) return;
    	if((r=RIGHTSON(a)) >= h.n) {
		if(GREATERTHAN(a,l))
			SWITCH(a,l);
		return;
		}
    	if(LESSEQUAL(a,r) && LESSEQUAL(a,l)) return;
    	if(LESSEQUAL(l,r)) {
		SWITCH(a,l);
		a = l;
		}
      	     else {
		SWITCH(a,r);
		a = r;
		}
	}
    }

#define UHASH(x)	ABS((((int) x) % UTBLSIZE))

List UnAct[UTBLSIZE];
int UnActCount = 111;

InitUnAct()
{
    int i;
    for(i=0;i<UTBLSIZE;i++)
	InitList(&(UnAct[i]));
    UnActCount = 0;
    }

FindNext(n)
register int n;
{
    register instance *p;
    register int i;

    for(i=0;i<16;i++) {
	p = (instance *) &(UnAct[UHASH(n+i)]);
	if(p->Link != NIL && p->Link->min == n+i) {
	    NextUnAct = MIN(n+i,Top);
	    return(NextUnAct);
	    }
	}
    NextUnAct = MIN(n+i,Top);
    return(NextUnAct);
    }

PutUnAct(e)
instance *e;
{
    register instance *p;
    register List *l;

    if(e->min < UnActMark)
	/*
	if(e->min == UnActMark-1)
	    /* Let's ignore floating point round off errors */
	    /*
	    e->min = UnActMark;
	  else
	  */
	    Error("Element submitted past deadline in PutUnAct",INTERNAL);
    UnActCount++;
    l = &(UnAct[UHASH(e->min)]);
    e->Link = NIL;
    if(l->Link == NIL) {
	l->Link = (Element *) e;
	return;
	}
    for(p = (instance *) l; p->Link != NIL; p=p->Link)
	if(e->min <= p->Link->min) {
	    e->Link = p->Link;
	    p->Link = e;
	    return;
	    }
    p->Link = e;
    return;
    }

instance *
GetUnAct(i)
int i;
{
    register instance *e;
    register List *l;

    if(i < UnActMark)
	Error("GetUnAct called in bad order",INTERNAL);
    UnActMark = i;
    l = &(UnAct[UHASH(i)]);

    e = (instance *) l->Link;
    if(e==NIL) return(NIL);
    if(e->min == i) {
	l->Link = (Element *) e->Link;
	e->Link = NIL;
	--UnActCount;
	return(e);
	}
    if(e->min > i) {
	return(NIL);
	}
    Error("UnActiveList is out of order",INTERNAL);
    return(NIL);
    }

CheckUnAct() {
    int i;
    for(i=0;i<UTBLSIZE;i++)
	if(UnAct[i].Link!=NIL) {
	    fprintf(stderr,"UnActiveList is NOT empty\n");
	    return;
	    }
    fprintf(stderr,"the un active list is empty\n");
    if(UnActCount != 0) {
	fprintf(stderr,"the count is %d\n",UnActCount);
	}
    }
