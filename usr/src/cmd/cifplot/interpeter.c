/*******************************************************************
*                                                                  *
*    File: CIFPLOT/interpeter.c                                    *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"
#include "alloc.h"

IMPORT Command *FindSymbol();
IMPORT State();
IMPORT PrintCommand();
IMPORT Error();
IMPORT Command *AddCmd();
IMPORT Command *MakeSymbol();
IMPORT StoreSymbol();
IMPORT Error();
IMPORT Free();
IMPORT MakeBBox();
IMPORT transform *MakeTransform();

Execute(command)
Command *command;
/* Execute is called by the parser to interpet all the top
 * level commands.		*/
{
    Command *q;
    int n;

    switch(command->type) {
	case COMMENT:
		/* Ignore comments */
		FreeCommand(command);
		break;
	case SYMBOL:
		/* Store symbols in the symbol table */
		n = State(command->Ctype.Symbl.SymNo);
		/* Only Store if it is not already stored */
		if( (n == NONEXIST) || (n == DELETED) )
			StoreSymbol(command);
		/* If the symbol was previously called it's status
		 * becomes USED so we 'Examine' all it's commands */
		if(command->Ctype.Symbl.status == UNEXAMINED) {
			command->Ctype.Symbl.status = ACTIVE;
			for(q=command->Ctype.Symbl.CStart; q!=NIL; q=q->CLink)
				Examine(q,command->Ctype.Symbl.SymNo);
			command->Ctype.Symbl.status = USED;
			}
		break;
	case ARRAY:
	case CALL:
		/* The call may use a new symbol so we call 'Examine'
		 * to compute bounding boxes and dependencies.   */
		Examine(command,-1);
		/* Fall through */
	default:
		/* Add the command to the pseudo-symbol 'prog' */
		prog = AddCmd(prog,command);
		break;
	}
    }

Examine(sym,n)
Command *sym;
int n;
/* 'Examine' sets up the data structures used to handle forward references
 * and symbol renaming. It is invoked any time a symbol becomes 'USED' or a
 * call is made on a symbol at the top level. (i.e. the call is not part of a 
 * symbol). It is called with a command and an integer n which is the symbol
 * the call is contained in. (If the call is at the top level n = -1)
 *
 * If the command is a call, the state of the symbol which is called is checked
 * to see if is UNUSED. Since this call changes the state of the symbol to
 * used this is recorded. Examine is called on all the commands of the symbol.
 * (This is necessary since 'Examine' must be called any time a symbol becomes
 * used.)
 *
 * The called symbol may be non-existent at the time of the call. (This is
 * an error condition since CIF requires all symbols definitions to be defined
 * before they are needed.) In this case a symbol header is created with the
 * status of 'UNDEFINED'.
 *
 * There are two other valid states the called symbol can be in, these are
 * 'USED' and 'UNDEFINED'. If the symbol is in some other state we know there
 * exists some inconsistancy in the data and the program is aborted.
 *
 * Thus far it appears that all examine does is call itself on other commands
 * whenever a symbol becomes used. The real work of this function, however,
 * is to record which symbol calls which. The number of the symbol that called
 * the symbol we are examining is passed as the parameter 'n'. If n is -1 we
 * are not interested in it. Any other value means this symbol was called by
 * symbol n. We then add symbol n to the symbol's 'backTrace' list. If the 
 * symbol is the redefined or deleted we know of all the dangling references
 * to this symbol.
 *
 * Further 'Examine' computes the bounding boxes for each command and checks
 * for recursion
 */
{
    Command *p,*q,*ptr;
    struct CCell *cell;
    char s[128];

    if (sym->type == ARRAY)
		Examine(sym->Ctype.Array.ACom,n);
    if (sym->type == CALL) {
		switch(State(sym->Ctype.Call.CallNo)) {
			case NONEXIST:
				sprintf(s,"Symbol %d Undefined",sym->Ctype.Call.CallNo);
				Error(s,FATAL);
				p = MakeSymbol(sym->Ctype.Call.CallNo,1,1);
				p->Ctype.Symbl.status = UNDEFINED;
				StoreSymbol(p);
				ClearBBox(&(p->CBBox));
				break;
			case UNUSED:
				p = FindSymbol(sym->Ctype.Call.CallNo);
				p->Ctype.Symbl.status = ACTIVE;
				for(q=p->Ctype.Symbl.CStart; q!=NIL; q=q->CLink)
					Examine(q,p->Ctype.Symbl.SymNo);
				p->Ctype.Symbl.status = USED;
				MakeBBox(&(p->CBBox),p);
				break;
			case USED:
			case UNDEFINED:
				p=FindSymbol(sym->Ctype.Call.CallNo);
				break;
			case ACTIVE:
				sprintf(s,"Symbol %d Called Recursively",
					      sym->Ctype.Call.CallNo);
				Error(s,FATAL);
				ClearBBox(&(sym->CBBox));
				return;
			default:
				sprintf(s,"Symbol %d is in an illegal state\n",
					sym->Ctype.Call.CallNo);
				Error(s,INTERNAL);
			}
		if (n != -1) {
		    /* If symbol 'n' is not on the backtrace list for
		     * 'p' put it there. */
		    if(p->Ctype.Symbl.backTrace->CCNo != n) {
			ptr = FindSymbol(n);
			cell = (struct CCell *) palloc(sizeof(struct CCell));
			cell->CCCom = ptr;
			cell->CCNo = n;
			cell->CCLink = p->Ctype.Symbl.backTrace;
			p->Ctype.Symbl.backTrace = cell;
			}
		    }
		/* Set the call commands Symbol pointer to symbol 'p' */
		sym->Ctype.Call.CSymb = p;
	     }
    /* Compute the bounding box for command 'sym' */
    MakeBBox(&(sym->CBBox),sym);
    }

InitInter()
{
    int *i;
    InitLayers();
    InitSymbol();
    ident = MakeTransform();
    ident->refs = 1;
    i = (int *) ident;
    *(i-1) = 0;		/* cannot be freed */
    ZeroBBox(&GWindow);
    prog = MakeSymbol(-1,1,1);
    }

InterSummary()
{
    /* Compute bounding box for prog */
    struct BBox bb;
    Examine(prog,-1);
    if(GWindow.xmin > GWindow.xmax) {
	BBoxTransform(&GWindow,&(prog->CBBox),GlobalTransform);
	}
    if(debug > 3) PrintCommand(prog,stdout);
    if(debug) fprintf(stderr,"***Interpeter Summary***\n");
    if(debug) fprintf(stderr,"Window xmin %d, xmax %d, ymin %d, ymax %d\n",
		(int) prog->CBBox.xmin,(int) prog->CBBox.xmax,
		(int) prog->CBBox.ymin,(int) prog->CBBox.ymax);
    if(GWindow.xmax < GWindow.xmin || GWindow.ymax < GWindow.ymin) {
	fprintf(stderr,"Nothing to plot\n");
	abort();
	}
    }
