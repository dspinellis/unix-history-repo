/*******************************************************************
*                                                                  *
*    File: CIFPLOT/print.c                                         *
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

IMPORT struct LCell *GetLayer();
IMPORT long time();
IMPORT char *ctime();

FILE *inter;

int SymbCount = 0;

#define PRINTED 113

Intermediate()
{
    long t;
    if(NULL == (inter = fopen(outfile,"w"))) {
	char a[128];
	sprintf(a,"Can't create %s",outfile);
	Error(a,RUNTIME);
	}

    fprintf(inter,"(CIF 2.UCB-1);\n");
    t = time(0);
    fprintf(inter,"(%s: %s%s);\n",getlogin(),ctime(&t),banner);
    fprintf(inter,"();\n");
    fprintf(inter,"();\n");
    PrintBBox(&(prog->CBBox),inter);

    PrintSymbol(prog,inter);
    fprintf(inter,"C %d;\nE\n",SymbCount-1);
    exit(0);
    }

PrintSymbol(symb,inter)
Command *symb;
FILE *inter;
{
   Command *p;

   if(symb->type != SYMBOL)
	Error("PrintSymbol call with non-symbol",INTERNAL);
   if(symb->Ctype.Symbl.status == PRINTED)
	return;

   for(p=symb->Ctype.Symbl.CStart; p!=NIL; p=p->CLink) {
	if( p->type == CALL)
		PrintSymbol(p->Ctype.Call.CSymb,inter);
	if(p->type == ARRAY) {
		PrintSymbol(p->Ctype.Array.ACom->Ctype.Call.CSymb,inter);
		CreateArray(p,inter);
		}
	}
    
    fprintf(inter,"DS %d;\n",SymbCount);
    PrintBBox(&(symb->CBBox),inter);
    fprintf(inter,"(Symbol #%d);\n",symb->Ctype.Symbl.SymNo);
    fprintf(inter,"(9 %s);\n",symb->Ctype.Symbl.SName);
    symb->Ctype.Symbl.Sid = SymbCount++;
    symb->Ctype.Symbl.status = PRINTED;

    for(p=symb->Ctype.Symbl.CStart; p!=NIL; p=p->CLink)
	PrintCommand(p,inter);
    fprintf(inter,"DF;\n");
    }

CreateArray(com,inter)
Command *com;
FILE *inter;
{
    int i;

    fprintf(inter,"DS %d;\n",SymbCount++);
    PrintBBox(&(com->CBBox),inter);
    for(i=0;i<com->Ctype.Array.Am;i++)
	fprintf(inter,"C %d T %d 0;\n",
	    com->Ctype.Array.ACom->Ctype.Call.CSymb->Ctype.Symbl.Sid,
	    i*(int) com->Ctype.Array.Adx);
    fprintf(inter,"DF;\n");
    fprintf(inter,"DS %d;\n",SymbCount++);
    PrintBBox(&(com->CBBox),inter);
    for(i=0;i<com->Ctype.Array.An;i++)
	fprintf(inter,"C %d T 0 %d;\n",
		SymbCount-2,i*(int) com->Ctype.Array.Ady);
    fprintf(inter,"DF;\n");
    com->Ctype.Array.As = SymbCount-1;
    }

PrintCommand(com,inter)
Command *com;
FILE *inter;
{
    switch(com->type) {
	case POLYGON:
		PrintLayer(com->level,inter);
		fprintf(inter,"P");
		PrintPath(com->Ctype.Path,inter);
		fprintf(inter,";\n");
		/*
    		PrintBBox(&(com->CBBox),inter);
		*/
		break;
	case WIRE:
		PrintLayer(com->level,inter);
		fprintf(inter,"W");
		/* Print the width */
		fprintf(inter," %d",(int) com->Ctype.Wire.WWidth);
		PrintPath(com->Ctype.Wire.WPath,inter);
		fprintf(inter,";\n");
		/*
    		PrintBBox(&(com->CBBox),inter);
		*/
		break;
	case FLASH:
		PrintLayer(com->level,inter);
		fprintf(inter,"R %d %d,%d;\n",
			(int) com->Ctype.Flash.fdia,
			(int) com->Ctype.Flash.fcenter.x,
			(int) com->Ctype.Flash.fcenter.y);
		break;
	case BOX:
		PrintLayer(com->level,inter);
		fprintf(inter,"B %d %d %d,%d %d,%d;\n",
			(int) com->Ctype.Box.blength,
			(int) com->Ctype.Box.bwidth,
			(int) com->Ctype.Box.bcenter.x,
			(int) com->Ctype.Box.bcenter.y,
			(int) com->Ctype.Box.bdirect.x,
			(int) com->Ctype.Box.bdirect.y);
		/*
    		PrintBBox(&(com->CBBox),inter);
		*/
		break;
	case TEXT:
		fprintf(inter,"2 \"%s\" ",com->Ctype.Text.TString);
		PrintTransform(com->Ctype.Text.TTrans,inter);
		fprintf(inter,";\n");
		break;
	case POINTNAME:
		fprintf(inter,"94 %s %d %d",com->Ctype.PointName.Name,
		    (int) com->Ctype.PointName.loc.x,
		    (int) com->Ctype.PointName.loc.y);
		if(strcmp(com->Ctype.PointName.Label,"all") != 0)
		    fprintf(inter," %s",com->Ctype.PointName.Label);
		fprintf(inter,";\n");
		break;
	case ARRAY:
		fprintf(inter,"C %d;\n",com->Ctype.Array.As);
		/*
    		fprintf(inter,"0A %d %d,%d %d,%d;\n",
				com->Ctype.Array.ACom->Ctype.Call.CSymb->Ctype.Symbl.Sid,
				com->Ctype.Array.An,com->Ctype.Array.Am,
				(int) com->Ctype.Array.Adx,
				(int) com->Ctype.Array.Ady);
				*/
		/*
    		PrintBBox(&(com->CBBox),inter);
		*/
		break;
	case CALL:
		fprintf(inter,"C %d",com->Ctype.Call.CSymb->Ctype.Symbl.Sid);
		PrintTransform(com->Ctype.Call.trans,inter);
		fprintf(inter,";\n");
		/*
    		PrintBBox(&(com->CBBox),inter);
		*/
		break;
	case SYMBOL:
		PrintSymbol(com,inter);
		break;
	default:
		Error("Unknown type of command in PrintCommand",INTERNAL);
	}
    }

PrintPath(p,f)
PointList *p;
FILE *f;
{
	for(; p != NIL; p = p->PLink) {
		fprintf(f," %d,%d",(int)p->pt.x,(int)p->pt.y);
		}
	}	

PrintBBox(bbox,f)
struct BBox *bbox;
FILE *f;
{
    fprintf(f,"(BB %d %d %d %d);\n",
		(int) bbox->xmin,(int) bbox->xmax,
		(int) bbox->ymin,(int) bbox->ymax);
    }

PrintTransform(trans,f)
transform *trans;
FILE *f;
{
    int i,j;

    /*
    fprintf(f," MT");
    for(i=0;i<3;i++) {
	for(j=0;j<2;j++)
	    fprintf(f," %f", trans->t[i][j]);
	}
	*/
    fprintf(f,"%s",trans->TransString);
    }

PrintLayer(n,f)
int n;
FILE *f;
{
    struct LCell *p;

    p = GetLayer(n);
    if(p == NIL)
	Error("Can't find layer name in PrintLayer",INTERNAL);
    fprintf(f,"L %s;\n",p->LName);
    }
