/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)1.recog.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include "1.incl.h"
#include "def.h"


recognize(type, ifflag)			/* if ifflag = 1, statement is if()type; otherwise is type */
int type, ifflag;				/* do whatever is needed for this statement */
	{
	int *arctype,   i,   sp;
	VERT num, num1, nest, loophead;
	extern long label();
	long *arclab;
	if (nlabs > 3) sp = nlabs; else sp = 3;
	arctype = challoc(sizeof(*arctype) * sp);  arclab = challoc(sizeof(*arclab) * sp);
	for( i=0; i < endbuf; i++)  {if (buffer[i] == '~')  buffer[i] = ' ';}
	loophead = nest = innerdo(label(0));
	if (DEFINED(nest))
		{
			/* this statement is last line of do loop */
		nest = ARC(nest,0);		/* nest is ITERVX of the innermost do ending here */
		}


	if (ifflag)
		{
		if (type == ungo)
			{
			arctype[0] = -2;
			arclab[0] = label(1);
			}
		else
			arctype[0] = 0;

		arctype[1] = (nest >= 0) ? nest : -2;
		arclab[1] = implicit;
		num1 = makenode(IFVX,TRUE,TRUE,label(0),2,arctype,arclab);
		PRED(num1) = pred;
		}

	arctype[0] = (nest >= 0) ? nest : -2;
	arclab[0] = implicit;

	switch(type)
		{
		case ungo:
			if (!ifflag)
				{
				connect(label(1),implicit);
				if (label(0) != implicit)  connect(label(1),label(0));
				}
			break;
		case RETVX:
		case STOPVX:
			if (type == RETVX)
				{
				if (retvert == UNDEFINED)
					retvert = makenode(type,FALSE,FALSE,implicit,0,arctype,arclab);
				num = retvert;
				}
			else
				{
				if (stopvert == UNDEFINED)
					stopvert = makenode(type,FALSE,FALSE,implicit,0,arctype,arclab);
				num = stopvert;
				}
			if (!ifflag)
				{
				fixvalue(implicit,num);
				clear(implicit);
				if (label(0) != implicit) fixvalue(label(0),num);
				}
			break;


		case contst:
			contin(label(0),loophead);
			break;

		case FMTVX:
			num = makenode(FMTVX,FALSE,TRUE,implicit,0,arctype,arclab);
			BEGCODE(num) = comchar + 1 - rtnbeg;
			ONDISK(num) = endline - endcom;
			if (label(0) != implicit)
				fixvalue(label(0),num);
			FMTLST = append(num,FMTLST);
			break;
		case STLNVX:
			if (DEFINED(stflag) && !ifflag && (label(0) == implicit))
				{
				++CODELINES(stflag);
				ONDISK(stflag) += endline - begline + 1;
				}
			else
				{
				num = makenode(STLNVX,!ifflag,!ifflag,label(0),1,arctype,arclab);
				if (!ifflag)
					{
					stflag = num;
					BEGCODE(num) = comchar + 1 - rtnbeg;
					ONDISK(num) = endline - endcom;
					CODELINES(num) = 1;
					}
				else
					{
					BEGCODE(num) = stcode;
					ONDISK(num) = FALSE;
					CODELINES(num) = 1;
					}
				}
			break;

		case DOVX:
			if (arctype[0] != -2) 
				{
				error("illegal do range, ","","");
				fprintf(stderr," between lines %d and %d\n",begline, endline);
				exit(1);
				}
			arctype[1] = UNDEFINED;
			num1 = makenode(DOVX,TRUE,TRUE,label(0),2,arctype,arclab);
			if (++doptr >= maxdo)
				{
				faterr("in parsing:\n","do loops nested deeper than allowed","");
				}
			dostack[doptr] = label(1);
			doloc[doptr] = num1;			/* stack link to node after loop */
			INC(num1) = inc;
			num = makenode(ITERVX,TRUE,FALSE,implicit,1,arctype,arclab);
			ARC(num1,0) = num;
			FATH(num) = UNDEFINED;	/* number of DOVX can change so leave UNDEFINED until later */
			break;
		case arithif:
			if (label(1) == label(2) || label(1) == 0L)
				makeif(1,label(0),concat(pred," > 0"),label(3),label(2));
			else if (label(1) == label(3) || label(3) == 0L)
				makeif(1,label(0),concat(pred," == 0"),label(2),label(1));
			else if (label(2) == label(3) || label(2) == 0L)
				makeif(1,label(0),concat(pred," < 0"),label(1),label(3));
			else
				{
				makeif(1,label(0),concat(pred," < 0"),label(1),implicit);
				makeif(1,implicit,concat(pred," == 0"),label(2),label(3));
				}
			break;

		case IOVX:
			if (endlab)
				{
				arctype[1] = -2;
				arclab[1] = endlab->labelt;
				}
			else
				arctype[1] = UNDEFINED;
			if (errlab)
				{
				arctype[2] = -2;
				arclab[2] = errlab->labelt;
				}
			else
				arctype[2] = UNDEFINED;
			num = makenode(IOVX,!ifflag,!ifflag,label(0),3,arctype,arclab);
			PRERW(num) = prerw;
			POSTRW(num) = postrw;
			if (reflab)
				addref(reflab->labelt, &FMTREF(num));
			else
				FMTREF(num) = UNDEFINED;
			break;

		case COMPVX:
				if (intcase)
					{
					num = compcase(ifflag);
					break;
					}
		case ASGOVX:
			for (i = 0; i < nlabs - 1; i++)
				{
				arctype[i] = -2;
				arclab[i] = label(nlabs-i-1);
				}
			num = makenode(type,!ifflag,!ifflag,label(0),nlabs - 1, arctype, arclab);
			EXP(num) = exp;
			break;
		case ASVX:
			num = makenode(ASVX,!ifflag,!ifflag,label(0),1,arctype,arclab);
			EXP(num) = exp;
			addref(label(1),&LABREF(num));
			break;
		case entry:
			num = makenode(STLNVX,FALSE,TRUE,label(0),1,arctype,arclab);
			BEGCODE(num) = comchar + 1 - rtnbeg;
			ONDISK(num) = endline - endcom;
			CODELINES(num) = 1;
			ENTLST = append(num,ENTLST);
			break;
		}
	if (ifflag && type != ungo)
		{
		ARC(num1,0) = num;
		}
	if (DEFINED(loophead))  nesteddo(label(0), loophead);
	if (ifflag || DEFINED(loophead) || type != STLNVX)  stflag = UNDEFINED;


	chfree(arctype,sizeof(*arctype) * sp);  chfree(arclab,sizeof(*arclab) * sp);
	if (debug)
		{
		fprintf(debfd,"line %d:  ", begline);
		if (ifflag) fprintf(debfd,"if()  ");
		switch(type)
			{case RETVX:	fprintf(debfd,"return");	break;
			case STOPVX:	fprintf(debfd,"stop");	break;
			case contst:	fprintf(debfd,"continue");	break;
			case ungo:	fprintf(debfd,"uncond. goto");	break;
			case COMPVX:	fprintf(debfd,"comp. goto");	break;
			case ASGOVX:	fprintf(debfd,"ass. goto, labs");	break;
			case ASVX:	fprintf(debfd,"label assignment");	break;
			case STLNVX:	fprintf(debfd,"simple statement");	break;
			case arithif:	fprintf(debfd,"arith if");	break;
			case DOVX:	fprintf(debfd,"do ");	break;
			case FMTVX:  fprintf(debfd,"format st");  break;
			case IOVX:  fprintf(debfd,"IOVX statement ");  break;
case entry:	fprintf(debfd,"entry statement ");  break;
			}
		fprintf(debfd,"\n%s\n", buffer);
		}
	}



makeif(first,labe,test,arc1,arc2)			/* construct IFVX with arcs to labels arc1,arc2 */
int first;
long labe, arc1,arc2;
char *test;
	{
	int num, arctype[2];
	long arclab[2];
	arctype[0] = arctype[1] = -2;
	arclab[0] = arc1;
	arclab[1] = arc2;
	num = makenode(IFVX,first,first,labe,2,arctype,arclab);
	PRED(num) = test;
	return(num);
	}


innerdo(labe)		/* return number of DOVX associated with labe, or UNDEFINED */
long labe;
	{
	if (DEFINED(doptr))
		{if (dostack[doptr] == labe)
			return(doloc[doptr--]);
		}
	return(UNDEFINED);
	}




contin(labe,nest)		/* handle continue statements */
long labe;
int nest;
	{
	VERT y;
	
	if (!DEFINED(nest))
		{		/* not nested */
		if (labe != implicit) connect(implicit,labe);	/* labe pts to next node */
		}
	else
		{		/* nested */
		y = ARC(nest,0);
		fixvalue(labe,y);			/* labe pts to ITERVX */
		fixvalue(implicit, y);		/* implicit links pt to ITERVX */
		clear(implicit);
		}
	}




nesteddo(labe,v)
			/* if multiple do's end on same label, add arc from inner DOVX
				to enclosing DOVX;
			add implicit link out of outermost DOVX with this label */
long labe;
int v;
	{
	
	while (DEFINED(doptr) && dostack[doptr] == labe)
		{
		ARC(v,1) = ARC(doloc[doptr],0);		/*set inner DOVX to point to outer ITERVX */
		v = doloc[doptr--];
		}
	addref(implicit, &ARC(v,1));
	}



compcase(ifflag)		/* turn computed goto into case statement */
LOGICAL ifflag;
	{
	int *arctype, i, num, d, arct;
	extern long label();
	long *arclab;
	char *str;
	arctype = challoc(sizeof(*arctype) * nlabs);
	arclab = challoc (sizeof(*arclab) * nlabs);

	d = distinct(linelabs->nxtlab,arctype,arclab,nlabs-1);
			/* puts distinct labels in arclab, count of each in arctype */
	arct = -2;
	for (i = 0; i < d; ++i)
		arctype[i] = makenode(ICASVX,FALSE,FALSE,implicit,1,&arct,&arclab[i]);
	num = makenode(SWCHVX,!ifflag,!ifflag,label(0),d,arctype,arclab);
	EXP(num) = exp;

	str = challoc(6*(nlabs-1));	/* 5 digits + , or \0 per label */
	for (i = 0; i < d; ++i)		/* construct list of values for each label */
		EXP(arctype[i]) = stralloc(str,accum(str,linelabs->nxtlab,arclab[i]));
	chfree(str,6*(nlabs-1));
	chfree(arctype,sizeof(*arctype) * nlabs);  chfree(arclab,sizeof(*arclab) * nlabs);
	return(num);
	}


accum(str,vlist,f)		/* build string of indices in compnode  corr. to label f */
char *str;  long f;  struct lablist *vlist;
	{
	int s,j;  struct lablist  *p;

	s = 0;
	j = 1;
	for (p = vlist; p ; p = p->nxtlab)		/* search for occurrences of f */
		{
		if (p->labelt ==f)
			{
			if (s)
				{
				str[s] = ',';
				++s;
				}
			sprintf(&str[s],"%d",j);
			while (str[s] != '\0') ++s;
			}
		++j;
		}
	return(s+1);
	}


distinct(vlist,count,dlist,size)		/* make dlist into list of distinct labels in vlist */
struct lablist *vlist;  long dlist[];		/*count[] gets count of each label;  d distinct labels */
int count[],size;
	{int d,i;
	d = 0;
	for(i = 0; i < size; i++)  count[i] = 0;

	for (;vlist && vlist->labelt != 0L; vlist = vlist ->nxtlab)
		{
		for (i = 0; ;i++)
			{
			if (i == d)  dlist[d++] = vlist->labelt;
			if (dlist[i] == vlist->labelt)
				{
				++count[i];  break;
				}
			}
		}
	return(d);
	}


