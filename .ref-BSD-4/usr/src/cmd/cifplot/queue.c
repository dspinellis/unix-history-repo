/*******************************************************************
*                                                                  *
*    File: CIFPLOT/queue.c                                         *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "out_structs.h"

IMPORT Element *GetList();

InitQueue(q)
Queue *q;
{
    q->QStart = NIL;
    }

PutQueue(e,q)
Queue *q;
Element *e;
/* Put element `e' into queue `q' */
{
    e->Link = NIL;
    if(q->QStart == NIL) {
	q->QStart = q->QEnd = e;
	return;
	}
    q->QEnd->Link = e;
    q->QEnd = e;
    return;
    }

Element *
GetQueue(q)
Queue *q;
/* Return the element at the front of the queue `q'
 * If the queue is empty return NIL */
{
    register Element *e;
    e = q->QStart;
    if(q->QStart != NIL) q->QStart = q->QStart->Link;
    return(e);
    }

CatQueue(q1,q2)
Queue *q1,*q2;
/* Tack q2 onto the end of q1 */
{
    if(q1->QStart == NIL)
	q1->QStart = q2->QStart;
     else
	q1->QEnd->Link = q2->QStart;
    if(q2->QStart != NIL)
    	q1->QEnd = q2->QEnd;
    }

MoveQueue(q1,q2)
Queue *q1,*q2;
/* Make q1 point to the same queue as q2 */
{
    q1->QStart = q2->QStart;
    q1->QEnd = q2->QEnd;
    }

InitList(l)
List *l;
{
    l->Link = NIL;
    }

PutList(e,l,f)
List *l;
Element *e;
real (*f)();
/* Insert element `e' into list `l' maintaining the ascending
 * order of the list where `f' is a function of two arguments
 * giving the ordinality of the elements.
 * So if two elements `i' & `j' are compared then
 * f(i,j) <= 0 iff i <= j.
 * If the ordering doesn't matter substitute 0 for f */
{
    Element *p;

    e->Link = NIL;
    if(f == 0) {
	e->Link = l->Link;
	l->Link = e;
	return;
	}
    if(l->Link == NIL) {
	l->Link = e;
	return;
	}
    
    for(p= (Element *) l;p->Link != NIL; p=p->Link)
	if((*f)(e,p->Link) <= 0.0) {
	    e->Link = p->Link;
	    p->Link = e;
	    return;
	    }
    p->Link = e;
    return;
    }

MergeActives(i,list)
int i;
List *list;
{
    Element *p,*e;

    if(ActiveEdges[i].Link == NIL) {
	ActiveEdges[i].Link = list->Link;
	list->Link = NIL;
	return;
	}
    e = GetList(list);
    if(e == NIL) return;
    
    for(p= (Element *) &(ActiveEdges[i]);p->Link != NIL; p=p->Link)
	if(((edge *) e)->iy <= ((edge *) p->Link)->iy) {
	    e->Link = p->Link;
	    p->Link = e;
    	    e = GetList(list);
    	    if(e == NIL) return;
	    }
    p->Link = e;
    e->Link = list->Link;
    list->Link = NIL;
    return;
    }

Element *
GetList(l)
List *l;
/* Return the element at the front of the list `l' */
{
    register Element *e;
    e = l->Link;
    if(e != NIL) l->Link = l->Link->Link;
    return(e);
    }

Element *
PeekList(l)
List *l;
/* return pointer to first element in list without removing it */
{
    return(l->Link);
    }

SortList(l,f)
List *l;
real (*f)();
/* sort list l with fuction f as in PutList */
{
    register Element *e,*t1,*t2;
    int sorted;

    if( (l->Link == NIL) || (l->Link->Link == NIL) ) return;
    sorted = 0;
    while(!sorted) {
	sorted = 1;
	for(e = (Element *) l; e->Link->Link != NIL; e=e->Link)
	    if((*f)(e->Link,e->Link->Link) > 0.0) {
		t1 = e->Link;
		t2 = t1->Link;
		t1->Link = t2->Link;
		t2->Link = t1;
		e->Link = t2;
		sorted = 0;
		}
	}
    }

int sortcnt = 0;

SortActives(i)
int i;
/* Does a bubble sort on ActiveList[i] */
{
    register Element *e,*t1,*t2;
    Element *last;
    int sorted;

    if( (ActiveEdges[i].Link == NIL) || (ActiveEdges[i].Link->Link == NIL) ) return;
    sorted = 0;
    last = NIL;
    while(!sorted) {
	sorted = 1;
	sortcnt++;
	for(e = (Element *) &(ActiveEdges[i]); e->Link->Link != last; e=e->Link) {
	    if(((edge *) e->Link)->iy > ((edge *) e->Link->Link)->iy) {
		change[i] = 1;
		t1 = e->Link;
		t2 = t1->Link;
		t1->Link = t2->Link;
		t2->Link = t1;
		e->Link = t2;
		sorted = 0;
		}
	    }
	last = t1;
	}
    }

SortSummary()
{
    printf("%d Sort Scans\n",sortcnt);
    }
