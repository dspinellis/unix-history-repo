/*******************************************************************
*                                                                  *
*    File: CIFPLOT/make.c                                          *
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
IMPORT Command *alloc();
IMPORT Command *palloc();
IMPORT struct LCell *FindLayer();
IMPORT Free();
IMPORT State();
IMPORT CopyDelete();
IMPORT Error();
IMPORT ZeroBBox();
IMPORT CompBBox();
IMPORT CompPtBBox();
IMPORT MakeBBox();
IMPORT transform *MakeTransform();
IMPORT char *Concat();

extern Command *MakeComment();	/* Forward Reference */

Command *
MakePoly(p)
struct PathHeader *p;
{
    Command *command;
	command = GetCommand();
	command->type = POLYGON;
	command->Ctype.Path = p->PHead;
	Free(p);
	command->CLink = NIL;
	MakeBBox(&(command->CBBox),command);
	return(command);
   }

Command *
MakeBox(length,width,center,direct)
real length,width;
point *center,*direct;
{
    Command *command;
    command = GetCommand();
    command->type = BOX;
    command->CLink = NIL;
    command->Ctype.Box.blength = length;
    command->Ctype.Box.bwidth = width;
    command->Ctype.Box.bcenter.x = center->x;
    command->Ctype.Box.bcenter.y = center->y;
    CheckPoint(direct);
    command->Ctype.Box.bdirect.x = direct->x;
    command->Ctype.Box.bdirect.y = direct->y;
    MakeBBox(&(command->CBBox),command);
    return(command);
    }

CheckPoint(pt)
point *pt;
{
    if( (pt->x == 0.0) && (pt->y == 0.0) ) {
	pt->x = 1.0;
	return(0);
	}
    return(1);
    }

Command *
MakeFlash(dia,center)
real dia;
point *center;
{
    Command *command;
    command = GetCommand();
    command->type = FLASH;
    command->CLink = NIL;
    command->Ctype.Flash.fdia = dia;
    command->Ctype.Flash.fcenter.x = center->x;
    command->Ctype.Flash.fcenter.y = center->y;
    MakeBBox(&(command->CBBox),command);
    return(command);
    }

Command *
MakeWire(width,p)
real width;
struct PathHeader *p;
{
    Command *command;
    command = GetCommand();
    command->type = WIRE;
    ZeroBBox(&(command->CBBox));
    command->CLink = NIL;
    command->Ctype.Wire.WWidth = width;
    command->Ctype.Wire.WPath = p->PHead;
    command->Ctype.Wire.WIns = NIL;
    Free(p);
    MakeBBox(&(command->CBBox),command);
    return(command);
    }

Command *
MakeSymbol(num,a,b)
int num,a,b;
{
    Command *command,*p;
    struct CCell *q;
    char s[128];

    switch(State(num)){
	case USED:
	case UNUSED:
		sprintf(s,"Symbol %d redefined",num);
		Error(s,WARNING);
		p = FindSymbol(num);
		p->Ctype.Symbl.status = DELETED;
		if(p->Ctype.Symbl.backTrace != NIL)
			Error("Dangling References after Symbol Redefintion",WARNING);
		for(q=p->Ctype.Symbl.backTrace; q!=NIL; q=q->CCLink)
			CopyDelete(q->CCCom);
		/* Fall through and create new symbol */
	case DELETED:
	case NONEXIST:
    		command = GetCommand();
    		command->type = SYMBOL;
    		command->Ctype.Symbl.SymNo = num;
    		command->Ctype.Symbl.backTrace = NIL;
		command->Ctype.Symbl.status = UNUSED;
    		command->Ctype.Symbl.a = a;
    		command->Ctype.Symbl.b = b;
		command->Ctype.Symbl.CStart = NIL;
		command->Ctype.Symbl.CFinnish = NIL;
		command->Ctype.Symbl.SName = Concat("",0);
    		command->CLink = NIL;
		command->level = -1;
    		ZeroBBox(&(command->CBBox));
		return(command);
	case UNDEFINED:
		command = FindSymbol(num);
		command->Ctype.Symbl.status = UNEXAMINED;
    		command->Ctype.Symbl.a = a;
    		command->Ctype.Symbl.b = b;
		return(command);
	default:
	  {     char s[128];
		sprintf(s,"Symbol %d in unknown state\n",num);
		Error(s,INTERNAL);
		}
	}
    }

Command *
AddCmd(h,c)
Command *h;
Command *c;
{
    Command *p;

    switch(c->type) {
	case COMMENT:
		FreeCommand(c);
		return(h);
	case NAME:
		if(*(h->Ctype.Symbl.SName) != '\0') {
			char s[256],*msg;
			sprintf(s,"Symbol %d has already been named ",
					h->Ctype.Symbl.SymNo);
			msg = Concat(s,h->Ctype.Symbl.SName,0);
			Error(msg,WARNING);
			Free(msg);
			h->Ctype.Symbl.SName = Concat(h->Ctype.Symbl.SName,
						"--",c->Ctype.s,0);
			FreeCommand(c);
			return(h);
			}
		h->Ctype.Symbl.SName = c->Ctype.s;
		FreeCommand(c);
		return(h);
	case LAYER:
		h->level = c->level;
		FreeCommand(c);
		return(h);
	case WIRE:
	case POLYGON:
	case BOX:
	case FLASH:
		/* Check for valid layer description */
		if(h->level == -1) {
		    Error("Layer not Specified",RECOVERABLE);
		    h->level = 0;
		    }
	case POINTNAME:
	case TEXT:
	case CALL:
	case ARRAY:
		/* Add new command to end of command list */
    		if (h->Ctype.Symbl.CStart == NIL)
			h->Ctype.Symbl.CFinnish = h->Ctype.Symbl.CStart = c;
     		    else {
			h->Ctype.Symbl.CFinnish->CLink = c;
			h->Ctype.Symbl.CFinnish = c;
			}
		/* New Command may itself be a list (i.e. a wire) */
		for(p=c; p!=NIL; p= p->CLink) {
		    h->Ctype.Symbl.CFinnish = p;
		    /* Set to current level */
		    p->level = h->level;
		    }
    		return(h);
	default:
	   {	char s[128];
		sprintf(s,"Unknown Command in Defintion of Symbol %d\n",
					h->Ctype.Symbl.SymNo);
		Error(s,INTERNAL);
		}
	}
    }

Command *
MakeCall(num,trans)
int num;
transform *trans;
{
    Command *command;
    command = GetCommand();
    command->type = CALL;
    command->Ctype.Call.CallNo = num;
    command->Ctype.Call.trans = trans;
    command->Ctype.Call.CSymb = NIL;
    ZeroBBox(&(command->CBBox));
    command->CLink = NIL;
    return(command);
    }

Command *
MakeComment()
{
    Command *command;
    command = GetCommand();
    command->type = COMMENT;
    ZeroBBox(&(command->CBBox));
    command->CLink = NIL;
    return(command);
    }

struct PathHeader *
MakePath(pt)
point *pt;
{
    PointList *path;
    struct PathHeader *PH;

    path = (PointList *) alloc(sizeof(PointList));
    PH = (struct PathHeader *) alloc(sizeof(struct PathHeader));
    PH->PNo = 1;
    PH->PHead = PH->PTail = path;
    path->PLink = NIL;
    path->pt.x = pt->x;
    path->pt.y = pt->y;
    return(PH);
    }

struct PathHeader *
AddPath(PH,pt)
struct PathHeader *PH;
point *pt;
{
    PointList *newpath;

    newpath = (PointList *) alloc(sizeof(PointList));
    (PH->PNo)++;
    PH->PTail->PLink = newpath;
    PH->PTail = newpath;
    newpath->PLink = NIL;
    newpath->pt.x = pt->x;
    newpath->pt.y = pt->y;
    return(PH);
    }


point *
MakePoint(x,y)
real x,y;
{
    point *p;
    p = (point *) alloc(sizeof(point));
    p->x = x;
    p->y = y;
    return(p);
    }

Command *
MakeArray(s,m,n,dx,dy)
int s,m,n;
real dx,dy;
{
    Command *command;
    transform *t;

    command = GetCommand();
    command->type = ARRAY;
    ZeroBBox(&(command->CBBox));
    command->CLink = NIL;
    t = MakeTransform();
    command->Ctype.Array.ACom = MakeCall(s,t);
    command->Ctype.Array.As = s;
    command->Ctype.Array.Am = m;
    command->Ctype.Array.An = n;
    command->Ctype.Array.Adx = dx;
    command->Ctype.Array.Ady = dy;
    return(command);
    }

Command *
MakePointName(s,pt,label)
char *s,*label;
point *pt;
{
    Command *command;
    command = GetCommand();
    command->type = POINTNAME;
    command->Ctype.PointName.Name = s;
    command->Ctype.PointName.loc.x = pt->x;
    command->Ctype.PointName.loc.y = pt->y;
    command->Ctype.PointName.Label = label;
    if(strcmp(label,"all") == 0)
	command->level = -1;
      else {
	struct LCell *p;
	if((p=FindLayer(label)) == NIL) {
	    Error("Unknown layer",RECOVERABLE);
	    command->level = -1;
	    }
	  else
	    command->level = p->LNum;
	}
    MakeBBox(&(command->CBBox),command);
    return(command);
    }

Command *
MakeName(s)
char *s;
{
    Command *command;
    command = GetCommand();
    command->type = NAME;
    ZeroBBox(&(command->CBBox));
    command->CLink = NIL;
    command->Ctype.s = s;
    return(command);
    }

Command *
MakeText(s,t,c)
char *s;
transform *t;
char c;
{
    Command *command;
    command = GetCommand();
    command->type = TEXT;
    command->Ctype.Text.TString = s;
    command->Ctype.Text.TTrans = t;
    command->Ctype.Text.TLoc = c;
    ZeroBBox(&(command->CBBox));
    command->CLink = NIL;
    return(command);
    }
