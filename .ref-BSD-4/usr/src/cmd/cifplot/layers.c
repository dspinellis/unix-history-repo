/*******************************************************************
*                                                                  *
*    File: CIFPLOT/layers.c                                        *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"
#include "pats.h"
#include "alloc.h"

IMPORT alloc();
IMPORT palloc();

struct LCell *Layers;	/* Points to the list of layers */
int UsedLayers = 0;	/* Count of how many layers have been used */

extern struct LCell *FindLayer();	/* Forward Reference FindLayer */

InitLayers()
{
    int i;

    MaxLayers = NUM_LAYERS;
    Pats = (int **) alloc(MaxLayers*sizeof(int *));
    Layers = NIL;
    for(i=0; i<NLAYERS; i++)
	CreatLayer(pats[i].IName,&(pats[i].IPat[0]));
    FindLayer(pats[0].IName);
    }

CreatLayer(s,p)
char *s;
int *p;
/* Add a layer to the 'Layers' list. 's' is the layer's name and
 * p is a pointer to the patterns */
{
    struct LCell *t;

    t = (struct LCell *) palloc(sizeof(struct LCell));
    t->LName = s;
    t->pat = p;
    t->LNum = -1;
    /* Add t to list of layers */
    t->Link = Layers;
    t->BackLink = NIL;
    t->visible = 1;
    if(Layers != NIL)
	Layers->BackLink = t;
    Layers = t;
    return;
    }

struct LCell *
FindLayer(s)
char *s;
/* Return a pointer to list element of layer 's'. Return NIL if
 * 's' is not a layer */
{
    struct LCell *p;

    for(p=Layers;p!=NIL;p=p->Link)
	if(strcmp(p->LName,s) == 0) {
	    if(p->LNum == -1) {
		/* If this layer has never been used before give it
		 * a number */
		if(UsedLayers >= MaxLayers) {
			MaxLayers = MaxLayers*2;
			Pats = (int **) expand(Pats,MaxLayers*sizeof(int *));
			}
		Pats[UsedLayers] = p->pat;
		p->LNum = UsedLayers++;
		}
	    return(p);
	    }
    return((struct LCell *) NIL);
    }

struct LCell *
GetLayer(n)
int n;
{
    struct LCell *p;

    for(p=Layers;p!=NIL;p=p->Link)
	if(p->LNum == n)
	    return(p);
     return(NIL);
}

Command *
MakeLayer(l)
char *l;
/* Create a Command of type LAYER which has the value of
 * the layer in it's level field */
{
    Command *command;
    struct LCell *p;

    command = GetCommand();
    command->type = LAYER;
    command->CLink = NIL;
    command->Ctype.Layer = l;
    p = FindLayer(l);
    if(p == NIL) {
	Error("Unknown Layer",FATAL);
	CreatLayer(l,FindLayer("null")->pat);
	p = FindLayer(l);
	}
    command->level = p->LNum;
    return(command);
    }

Invisible(str)
char *str;
/* str is a string of layer names seperated by commas or spaces
 * that are to be made invisible */
 {
    char *name,ch;
    struct LCell *l;
    do {
	name = str;
	while(*str != '\0' && *str != ',' && *str != ' ' && *str != '\t') str++;
	ch = *str;
	*str = '\0';
	if(strcmp(name,"bbox") == 0) symbox = 0;
	  else if(strcmp(name,"text") == 0) text = 0;
	    else if(strcmp(name,"symbolName") == 0) printSymbolName = 0;
	      else {
		l = FindLayer(name);
		if(l != NIL) l->visible = 0;
		  else fprintf(stderr,"%s: No Such Layer\n",name);
		}
	str++;
	} while(ch != '\0');
    }
