#ifndef lint
static char sccsid[] = "@(#)4.out.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#include "def.h"
#include "4.def.h"
#include "3.def.h"

outrat(v,tab,tabfirst)
VERT v;
int tab;		/* number of tabs to indent */
LOGICAL tabfirst;	/* FALSE if doing IF of ELSE IF */
	{
	LOGICAL ndcomma;
	VERT w;
	int type,i;
	type = NTYPE(v);
	if (hascom[type])
		prcom(v);
	if (!LABEL(v) && type == FMTVX)
		{
		OUTSTR("#following unreferenced format statement commented out\n");
		OUTSTR("#");
		}
	if (LABEL(v) && type != ITERVX)
		{
		ASSERT(tabfirst, outrat);
		prlab(LABEL(v),tab);
		}
	else if (tabfirst && type != DUMVX && type != ITERVX)
		TABOVER(tab);

	switch(type)
		{
		case DUMVX:
			newlevel(v,0,tab,YESTAB);
			break;
		case GOVX:
			OUTSTR("go to ");
			OUTNUM(LABEL(ARC(v,0)));
			OUTSTR("\n");
			break;
		case STOPVX:
			if (progtype != blockdata)
				OUTSTR("stop\n");
			break;
		case RETVX:
			OUTSTR("return\n");
			break;
		case BRKVX:
			if (!levbrk)
				{
				ASSERT(LEVEL(v) == 1,outrat);
				OUTSTR("break\n");
				}
			else
				{
				OUTSTR("break ");
				OUTNUM(LEVEL(v));
				OUTSTR("\n");
				}
			break;
		case NXTVX:
			if (!levnxt)
				{
				ASSERT(LEVEL(v) == 1,outrat);
				OUTSTR("next\n");
				}
			else
				{
				OUTSTR("next ");
				OUTNUM(LEVEL(v));
				OUTSTR("\n");
				}
			break;
		case ASGOVX:
		case COMPVX:
			OUTSTR("goto ");
			if (type == ASGOVX)
				{
				OUTSTR(EXP(v));
				OUTSTR(",");
				}
			OUTSTR("(");
			for (i = ARCNUM(v)-1; i >=0; --i)		/* arcs were stored backward */
				{
				OUTNUM(LABEL(ARC(v,i)));
				if (i > 0) OUTSTR(",");
				}
			OUTSTR(")");
			if (type == COMPVX)
				{
				OUTSTR(",");
				OUTSTR(EXP(v));
				}
			OUTSTR("\n");
			break;
		case ASVX:
			OUTSTR("assign ");
			OUTNUM(LABEL(LABREF(v)));
			OUTSTR(" to ");
			OUTSTR(EXP(v));
			OUTSTR("\n");
			break;
		case IFVX:
			OUTSTR("IF");
			prpred(v,TRUE);
			if (IFTHEN(v))
				newlevel(v,THEN,tab+1,YESTAB);
			else
				{
				newlevel(v,THEN,tab+1,YESTAB);
				TABOVER(tab);
				OUTSTR("ELSE ");
				w = LCHILD(v,ELSE);
				ASSERT(DEFINED(w),outrat);
				if (NTYPE(w) == IFVX && !LABEL(w) && !DEFINED(RSIB(w)) &&
					!HASBRACE(v,ELSE) )
					newlevel(v,ELSE,tab,NOTAB);
				else
					newlevel(v,ELSE,tab+1,YESTAB);
				}
			break;
		case ITERVX:
			newlevel(v,0,tab,YESTAB);
			ASSERT(DEFINED(NXT(v)),outrat);
			if (LABEL(NXT(v)))
				{
				prlab(LABEL(NXT(v)),tab);
				OUTSTR("continue\n");
				}
			break;
		case DOVX:
			OUTSTR("DO ");
			OUTSTR(INC(v));
			newlevel(v,0,tab+1,YESTAB);
			break;
		case LOOPVX:
		case UNTVX:
			OUTSTR("REPEAT");
			newlevel(v,0,tab+1,YESTAB);
			if (type == UNTVX)
				{
				TABOVER(tab+1);
				OUTSTR("UNTIL");
				ASSERT(DEFINED(ARC(v,0)),outrat);
				prpred(LPRED(ARC(v,0)),TRUE);
				OUTSTR("\n");
				}
			break;
		case WHIVX:
			OUTSTR("WHILE");
			ASSERT(DEFINED(ARC(v,0)),outrat);
			ASSERT(DEFINED(LPRED(ARC(v,0))),outrat);
			prpred(LPRED(ARC(v,0)),TRUE);
			newlevel(v,0,tab+1,YESTAB);
			break;
		case STLNVX:
		case FMTVX:
			prstln(v,tab);
			break;
		case SWCHVX:
				OUTSTR("SWITCH");
				if (DEFINED(EXP(v)))
					{
					OUTSTR("(");
					OUTSTR(EXP(v));
					OUTSTR(")");
					}
				newlevel(v,0,tab+1,YESTAB);
				break;
		case ICASVX:
		case ACASVX:
			OUTSTR("CASE ");
			if (type == ACASVX)
				prpred(v,FALSE);
			else
				OUTSTR(EXP(v));
			OUTSTR(":\n");
			newlevel(v,0,tab+1,YESTAB);
			if (type == ACASVX &&DEFINED(LCHILD(v,ELSE)))
				{
				TABOVER(tab);
				OUTSTR("DEFAULT:\n");
				newlevel(v,1,tab+1,YESTAB);
				}
			break;
		case IOVX:
			OUTSTR(PRERW(v));
			ndcomma = FALSE;
			if (DEFINED(FMTREF(v)))
				{
				OUTNUM(LABEL(FMTREF(v)));
				ndcomma = TRUE;
				}
			if (DEFINED(ARC(v,ENDEQ)))
				{
				if (ndcomma) 
					OUTSTR(",");
				OUTSTR("end = ");
				OUTNUM(LABEL(ARC(v,ENDEQ)));
				ndcomma = TRUE;
				}
			if (DEFINED(ARC(v,ERREQ)))
				{
				if (ndcomma)
					OUTSTR(",");
				OUTSTR("err = ");
				OUTNUM(LABEL(ARC(v,ERREQ)));
				ndcomma = TRUE;
				}
			OUTSTR(POSTRW(v));
			OUTSTR("\n");
			break;
		}
	}


newlevel(v,ch,tab,tabfirst)
VERT v;
int ch;		/* number of lchild of v being processed */
int tab;		/* number of tabs to indent */
LOGICAL tabfirst;	/* same as for outrat */
	{
	LOGICAL addbrace;
	VERT w;
	if (NTYPE(v) == ACASVX || NTYPE(v) == ICASVX)
		addbrace = FALSE;
	else
		addbrace = HASBRACE(v,ch);
	ASSERT(tabfirst || !addbrace,newlevel);
	if (addbrace)
		OUTSTR(" {");
	if(tabfirst && NTYPE(v)!=ITERVX && NTYPE(v)!=DUMVX) OUTSTR("\n");
	for (w = LCHILD(v,ch); DEFINED(w); w = RSIB(w))
		outrat(w,tab,tabfirst);
	if (addbrace)
		{
		TABOVER(tab);
		OUTSTR("}\n");
		}
	}





prpred(v,addpar)
VERT v;
LOGICAL addpar;
	{
	if (addpar)
		OUTSTR("(");
	if (NEG(v)) OUTSTR("!(");
	OUTSTR(PRED(v));
	if (NEG(v)) OUTSTR(")");
	if (addpar)
		OUTSTR(")");
	}

prlab(n,tab)
int n,tab;
	{
	TABOVER(tab);
	OUTSTR("~");
	OUTNUM(n);
	OUTSTR(" ");
	}

prstln(v,tab)
VERT v;
int tab;
	{
	ASSERT(NTYPE(v) == STLNVX || NTYPE(v) == FMTVX,prstln);
	if (!ONDISK(v))
		{
		OUTSTR(BEGCODE(v));
		OUTSTR("\n");
		}
	else
		{
		empseek(BEGCODE(v));
		prcode(ONDISK(v),tab);
		}
	}

prcom(v)
VERT v;
	{
	if (DEFINED(BEGCOM(v)))
		{
		empseek(BEGCOM(v));
		comprint();
		}
	}
