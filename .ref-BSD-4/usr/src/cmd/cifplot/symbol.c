/*******************************************************************
*                                                                  *
*    File: CIFPLOT/symbol.c                                        *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"

extern Command *FindSymbol();	/* Forward reference FindSymbol */
IMPORT Command *MakeSymbol();
IMPORT Command *MakePoly();
IMPORT Command *MakeWire();
IMPORT Command *MakeBox();
IMPORT Command *MakeFlash();
IMPORT Command *MakeText();
IMPORT Command *MakeCall();
IMPORT Command *MakeArray();
IMPORT Command *MakePointName();
IMPORT struct PathHeader *MakePath();
IMPORT Error();
IMPORT AddCmd();

CopyDelete(sym)
Command *sym;
/* Make a copy of the symbol 'sym' and mark the original deleted.
 * Go down 'sym' backtrace list and CopyDelete all the symbols on that */
{
    struct CCell *p;
    int b;

    b = 0;
    if (sym->Ctype.Symbl.status != DELETED) {
	sym->Ctype.Symbl.status = DELETED;
	b = 1;
	CopySymbol(sym);
	for(p=sym->Ctype.Symbl.backTrace; p!=NIL; p=p->CCLink)
		CopyDelete(p->CCCom);
	}
    return(b);
    }

CopySymbol(sym)
Command *sym;
/* Make a new symbol header and copy all the Commands contained in 'sym'. */
{
    Command *newsym,*q,*p;
    PointList *ptr;
    struct PathHeader *path;
    point *pt;

    newsym = MakeSymbol(sym->Ctype.Symbl.SymNo,sym->Ctype.Symbl.a,
						sym->Ctype.Symbl.b);
    newsym->Ctype.Symbl.status = UNUSED;
    newsym->Ctype.Symbl.SName = sym->Ctype.Symbl.SName;
    for(p=sym->Ctype.Symbl.CStart; p!=NIL; p=p->CLink) {
	switch(p->type) {
		case POLYGON:
			pt = &(p->Ctype.Path->pt);
			path = MakePath(pt);
			for(ptr=p->Ctype.Path->PLink; ptr!=NIL; ptr= ptr->PLink)
				AddPath(path,&(ptr->pt));
			q = MakePoly(path);
			q->level = p->level;
			AddCmd(newsym,q);
			break;
		case WIRE:
			pt = &(p->Ctype.Wire.WPath->pt);
			path = MakePath(pt);
			for(ptr=p->Ctype.Wire.WPath->PLink; ptr!=NIL; ptr= ptr->PLink)
				AddPath(path,&(ptr->pt));
			q = MakeWire(p->Ctype.Wire.WWidth,path);
			q->level = p->level;
			AddCmd(newsym,q);
			break;
		case BOX:
			q = MakeBox(p->Ctype.Box.blength,p->Ctype.Box.bwidth,
				    &(p->Ctype.Box.bcenter),&(p->Ctype.Box.bdirect));
			AddCmd(newsym,q);
			break;
		case FLASH:
			q = MakeFlash(p->Ctype.Flash.fdia,&(p->Ctype.Flash.fcenter));
			AddCmd(newsym,q);
			break;
		case TEXT:
			q = MakeText(p->Ctype.Text.TString,p->Ctype.Text.TTrans);
			AddCmd(newsym,q);
			break;
		case POINTNAME:
			q = MakePointName(p->Ctype.PointName.Name,
				p->Ctype.PointName.loc.x,
				p->Ctype.PointName.loc.y,
				p->Ctype.PointName.Label);
			AddCmd(newsym,q);
			break;
			/*
		case POLYGON:
		case BOX:
		case WIRE:
		case FLASH:
		case TEXT:
			AddCmd(newsym,q);
			break;
			*/
		case ARRAY:
			q = MakeArray(p->Ctype.Array.As,
					p->Ctype.Array.Am,p->Ctype.Array.An,
					p->Ctype.Array.Adx,p->Ctype.Array.Ady);
			AddCmd(newsym,q);
			break;
		case CALL:
			q = MakeCall(p->Ctype.Call.CallNo,p->Ctype.Call.trans);
			AddCmd(newsym,q);
			break;
		default:
		   {	char s[128];
			sprintf(s,"Illegal command found in symbol %d\n",
						sym->Ctype.Symbl.SymNo);
			Error(s,INTERNAL);
			}
		}
	}
    StoreSymbol(newsym);
    }

Command *SymbolTable[TableSize];

#define hash(x)		ABS(x % TableSize)

StoreSymbol(sym)
Command *sym;
/* Put 'sym' in to the Symbol hash table */
{
    int n,status;

    n = sym->Ctype.Symbl.SymNo;
    /* Check to see there is no other copy of n */
    status = State(n);
    if( (status != NONEXIST) && (status != DELETED) ) {
	char s[128];
	sprintf(s,"Two Living Copies of Symbol %d",n);
	Error(s,INTERNAL);
	}

    sym->CLink = SymbolTable[hash(n)];
    SymbolTable[hash(n)] = sym;
    }

Command *
FindSymbol(n)
int n;
/* Return a pointer to symbol n */
{
    Command *p;

    for(p = SymbolTable[hash(n)]; p != NIL; p= p->CLink)
	if (p->Ctype.Symbl.SymNo == n) {
		return(p);
		}
    DEBUG("FindSymbol failed to return symbol\n");
    return(NIL);
    }

DeleteDefintion(i)
int i;
{
    int j,p;
    Command *temp,*store,*RemovedSymbols;
    struct CCell *bptr;

    RemovedSymbols = NIL;
    /* Go through every symbol in the symbol table
     * If the symbol is greater or equal to 'i' delete
     * the symbol and put it in the RemovedSymbol list.
     * If the symbol is less than 'i' put in a store list.
     * This reverses the order so put back in SymbolTable again
     * reversing the order
     */
    for(j=0; j<TableSize; j++) {
	store = NIL;
	for(temp=SymbolTable[j]; temp!= NIL; temp=SymbolTable[j]){
	    SymbolTable[j] = temp->CLink;
	    if(temp->Ctype.Symbl.SymNo < i) {
		temp->CLink = store;
		store = temp;
		}
	      else {
		temp->Ctype.Symbl.status = DELETED;
		temp->CLink = RemovedSymbols;
		RemovedSymbols = temp;
		}
	    }
	for(temp=store; temp != NIL; temp=store) {
	    store = temp->CLink;
	    temp->CLink = SymbolTable[j];
	    SymbolTable[j] = temp;
	    }
	}
    /* Now CopyDelete all the non-deleted backtrace symbols */
    p = 0;
    for(temp = RemovedSymbols; temp!=NIL; temp=temp->CLink)
	for(bptr=temp->Ctype.Symbl.backTrace;bptr!=NIL;bptr=bptr->CCLink)
	    p |= CopyDelete(bptr->CCCom);
    if(p) Error("Dangling References After Delete Defintion",WARNING);
    }

State(n) 
int n;
/* Return the state of symbol n */
{
    Command *p;

    for(p = SymbolTable[hash(n)]; p != NIL; p = p-> CLink)
	if (p->Ctype.Symbl.SymNo == n)
		return(p->Ctype.Symbl.status);
    return(NONEXIST);
    }


InitSymbol(){
    int i;

    for(i=0;i<TableSize;i++)
	SymbolTable[i] = NIL;
    }
