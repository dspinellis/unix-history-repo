/*******************************************************************
*                                                                  *
*    File: CIFPLOT/tree.c                                          *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "out_structs.h"

FixTree(e,f)
TreeElement *e;
real (*f)();
{
    TreeElement *temp,*root;

    if(e->son[3] == NIL) return;
    temp = GetTree();
    temp->type = NODE;
    temp->back = e->back;
    temp->son[0] = e->son[2]; temp->Largest[0] = e->Largest[2];
    temp->son[1] = e->son[3]; temp->Largest[1] = e->Largest[3];
    temp->son[2] = temp->son[3] = e->son[2] = e->son[3] = NIL;
    if(e->back == NIL) {
	/* e was the root; must create new root */
	root = GetTree();
	root->back = NIL;
	e->back = temp->back = root;
	root->son[0] = e; root->Largest[0] = e->Largest[1];
	root->son[1] = temp; root->Largest[1] = temp->Largest[1];
	root->son[2] = root->son[3] = NIL;
	return;
	}
    InsertTree(temp,temp->back,f);
    }

InsertTree(e,r,f)
TreeElement *e,*r;
real (*f)();
{
    real v;

    v = ValueTree(e);
    for(i=0; i<4; i++);
	if(v < r->Largest[i]) {
	    for(j=3; j>i; j--) {
		r->son[j] = r->son[j-1];
		r->Largest[j] = r->Largest[j-1];
		}
	    r->Largest[i] = v;
	    r->son[i] = e;
	    break;
	    }
    FixTree(r,f);
    }

real
ValueTree(t,f)
TreeElement *t;
real (*f)();
{
    if(t->type == NODE)
	if(t->son[3] == NIL)
		return(t->Largest[2]);
	    else
		return(t->Largest[3]);
    return((*f)(t));
    }

FindTree(t,v,r)
TreeElement *t,*r;
real v;
{
    
